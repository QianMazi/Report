suppressMessages(library(RFactorModel))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))

# lcdb.update.QT_IndexTiming(type='build')
# lcdb.update.QT_IndexTiming(type='addindex',indexset = c('EI399330','EI801003'))
# lcdb.update.QT_IndexTiming(type='update')

getValue <- function(index,valtype=c('PE','PB'),caltype=c('median','mean'),begT=as.Date('2005-01-04'),endT){
  valtype <- match.arg(valtype)
  caltype <- match.arg(caltype)

  qr <- paste("select indexID,date,value from QT_IndexTiming
              where indexID=",QT(index)," and date>=",rdate2int(begT),
              " and date<=",rdate2int(endT),
              "and valtype=",QT(valtype)," and caltype=",QT(caltype))
  re <- dbGetQuery(db.local(),qr)
  if(empty(re)){
    qr <- paste("select convert(varchar(8),i.PubDate,112) 'PubDate'
        from dbo.LC_IndexBasicInfo i,dbo.SecuMain s
        where i.IndexCode=s.InnerCode and s.SecuCode=",QT(str_sub(index,3,8)))
    indexPubDate <- sqlQuery(db.jy(),qr)
    indexPubDate <- intdate2r(indexPubDate$PubDate)
    
    RebDates <- getRebDates(indexPubDate,Sys.Date()-1,'day')
    TS <- getComps(index,endT = RebDates,datasrc ='ts')
    value <- calcfun(TS,valtype,caltype)
    dbWriteTable(db.local(),'QT_IndexTiming',value,overwrite=FALSE,append=TRUE,row.names=FALSE)
  }

  if(begT<=indexPubDate) begT <- indexPubDate

}


lcdb.update.QT_IndexTiming <- function(type=c('update','build','addindex'),indexset=NA){
  type <- match.arg(type)
  
  subfun <- function(indexDate){
    tmp <- str_c('(',str_c(QT(str_sub(indexDate$indexID,3,8)),collapse = ","),')')
    
    #get index component
    qr <- paste("select 'EI'+s1.SecuCode 'indexID','EQ'+s2.SecuCode 'stockID',
                convert(varchar(8),l.InDate,112) 'InDate',
                convert(varchar(8),l.OutDate,112) 'OutDate'
                from LC_IndexComponent l
                LEFT join SecuMain s1 on l.IndexInnerCode=s1.InnerCode
                LEFT join SecuMain s2 on l.SecuInnerCode=s2.InnerCode
                where s1.SecuCode in",tmp,
                " order by s1.SecuCode,l.InDate")
    indexComp <- sqlQuery(db.jy(),qr)
    dbWriteTable(db.local(), name="amtao_tmp", value=indexComp, row.names = FALSE, overwrite = TRUE)
    
    #correct begT
    tmpdate <- ddply(indexComp,.(indexID),summarise,mindate=intdate2r(min(InDate)))
    indexDate <- merge(indexDate,tmpdate,by='indexID')
    indexDate$begT <- as.Date(ifelse(indexDate$begT<indexDate$mindate,indexDate$mindate,indexDate$begT))
    indexDate <- indexDate[,c("indexID","indexName","begT","endT")]
    
    for(i in 1:nrow(indexDate)){
      ptm <- proc.time()
      cat(i,':',as.character(indexDate$indexID[i]),rdate2int(indexDate$begT[i]),'\n')

      tmpdate <- rdate2int(getRebDates(indexDate$begT[i],indexDate$endT[i],'day'))
      tmpdate <- data.frame(date=tmpdate)
      dbWriteTable(db.local(), name="yrf_tmp", value=tmpdate, row.names = FALSE, overwrite = TRUE)                      
      qr <- paste("SELECT a.date as date, b.stockID from yrf_tmp a, amtao_tmp b
                  where b.IndexID=", QT(indexDate$indexID[i]), 
                  "and b.InDate<=a.date and (b.OutDate>a.date or b.OutDate IS NULL)")     
      TS <- dbGetQuery(db.local(),qr)
      TS$date <- intdate2r(TS$date)
      if(i==1){
        qr <- paste("SELECT f.trddate 'date',f.code 'stockID',f.pettm,f.pbmrq
                    FROM fsfactor f where f.trddate>=",rdate2int(min(indexDate$begT)), 
                    " and f.trddate<=",rdate2int(max(indexDate$endT)))
        alldata <- sqlQuery(db.quant(),qr)
        alldata$date <- intdate2r(alldata$date)
      }
      

      TSF <- merge.x(TS,alldata,by=c('date','stockID'))
      TSF <- TSF[!is.na(TSF$pettm),]
      TSF <- TSF[!is.na(TSF$pbmrq),]
      
      #pe median
      indexvalue <- ddply(TSF,.(date),summarise,value=median(pettm))
      indexvalue <- cbind(indexID=indexDate$indexID[i],indexName=indexDate$indexName[i],indexvalue,valtype='PE',caltype='median')
      
      #pe mean
      tmp <- TSF[,c('date','stockID','pettm')]
      tmp <- tmp[tmp$pettm>0 & tmp$pettm<1000,]
      colnames(tmp) <- c('date','stockID','factorscore')
      tmp <- RFactorModel:::factor.outlier(tmp,3)
      tmp <- ddply(tmp,.(date),summarise,value=mean(factorscore))
      tmp <- cbind(indexID=indexDate$indexID[i],indexName=indexDate$indexName[i],tmp,valtype='PE',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp)
      
      #pb median
      tmp <- ddply(TSF,.(date),summarise,value=median(pbmrq))
      tmp <- cbind(indexID=indexDate$indexID[i],indexName=indexDate$indexName[i],tmp,valtype='PB',caltype='median')
      indexvalue <- rbind(indexvalue,tmp)
      
      #pe mean
      tmp <- TSF[,c('date','stockID','pbmrq')]
      tmp <- tmp[tmp$pbmrq>0,]
      colnames(tmp) <- c('date','stockID','factorscore')
      tmp <- RFactorModel:::factor.outlier(tmp,3)
      tmp <- ddply(tmp,.(date),summarise,value=mean(factorscore))
      tmp <- cbind(indexID=indexDate$indexID[i],indexName=indexDate$indexName[i],tmp,valtype='PB',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp)

      
      if(i==1){
        re <- indexvalue
      }else{
        re <- rbind(re,indexvalue)
      }
      cat('cost ',(proc.time() - ptm)[3]/60,'min.\n')
    }
    return(re)
  }#subfun finished
  
  
  if(type=='build'){
    indexset <- c('EI000016','EI399005','EI399006','EI000300','EI000905',
                  'EI000805','EI000979','EI000808','EI000819','EI000998','EI000827',
                  'EI000934','EI399959','EI000933','EI000932','EI801120')
    
    tmp <- str_c('(',str_c(QT(str_sub(indexset,3,8)),collapse = ","),')')
    qr <- paste("select 'EI'+s.SecuCode 'indexID',s.SecuAbbr 'indexName',
                convert(varchar(8),i.PubDate,112) 'begT'
                from LC_IndexBasicInfo i,SecuMain s
                where i.IndexCode=s.InnerCode and s.SecuCode in",tmp)
    indexDate <- sqlQuery(db.jy(),qr)
    indexDate$begT <- intdate2r(indexDate$begT)
    indexDate[indexDate$begT<as.Date('2005-01-04'),'begT'] <- as.Date('2005-01-04')
    indexDate$endT <- Sys.Date()-1
    

    re <- subfun(indexDate)
    re$date <- rdate2int(re$date)
    dbWriteTable(db.local(),'QT_IndexTiming',re,overwrite=T,append=F,row.names=FALSE)
    return('Done!')
    
  }else if(type=='update'){
    begT <- dbGetQuery(db.local(),"select max(date) 'date' from QT_IndexTiming")
    begT <- trday.offset(intdate2r(begT$date),by=days(1))
    endT <- trday.offset(Sys.Date(),by=days(-1))
    if(begT>endT){
      return('Done!')
    }else{
      indexDate <- dbGetQuery(db.local(),"select distinct indexID,indexName from QT_IndexTiming")
      indexDate$begT <- begT
      indexDate$endT <- endT
      
      re <- subfun(indexDate)
      re$date <- rdate2int(re$date)
      dbWriteTable(db.local(),'QT_IndexTiming',re,overwrite=F,append=T,row.names=FALSE)
      return('Done!')
    }

  }else{
    old <- dbGetQuery(db.local(),"select distinct indexID from QT_IndexTiming")
    old <- c(old$indexID)
    indexset <- setdiff(indexset,old)
    
    tmp <- str_c('(',str_c(QT(str_sub(indexset,3,8)),collapse = ","),')')
    qr <- paste("select 'EI'+s.SecuCode 'indexID',s.SecuAbbr 'indexName',
                convert(varchar(8),i.PubDate,112) 'begT'
                from LC_IndexBasicInfo i,SecuMain s
                where i.IndexCode=s.InnerCode and s.SecuCode in",tmp)
    indexDate <- sqlQuery(db.jy(),qr)
    indexDate$begT <- intdate2r(indexDate$begT)
    indexDate[indexDate$begT<as.Date('2005-01-04'),'begT'] <- as.Date('2005-01-04')
    endT <- dbGetQuery(db.local(),"select max(date) 'date' from QT_IndexTiming")
    indexDate$endT <- intdate2r(endT$date)
    
    re <- subfun(indexDate)
    re$date <- rdate2int(re$date)
    dbWriteTable(db.local(),'QT_IndexTiming',re,overwrite=F,append=T,row.names=FALSE)
    return('Done!')
    
  }
}




