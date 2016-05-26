suppressMessages(library(RFactorModel))
suppressMessages(library(plyr))
suppressMessages(library(stringr))

getValue <- function(index='EI399006',valtype=c('PE','PB'),caltype=c('median','mean','wgtmean'),begT=as.Date('2006-01-04'),endT=Sys.Date()-300){
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
}


lcdb.update.QT_IndexTiming <- function(type=c('update','build','addindex'),indexset=NA){
  type <- match.arg(type)
  if(type=='build'){
    indexset <- c('EI801003','EI000016','EI000300','EI000905','EI399005','EI399006',
                  'EI000805','EI000979','EI000808','EI000819','EI000998','EI000827',
                  'EI000934','EI399959','EI000933','EI000932','EI801120')
    
    tmp <- str_c('(',str_c(QT(str_sub(indexset,3,8)),collapse = ","),')')
    qr <- paste("select 'EI'+s.SecuCode 'indexID',s.SecuAbbr 'indexName',
                convert(varchar(8),i.PubDate,112) 'PubDate'
                from LC_IndexBasicInfo i,SecuMain s
                where i.IndexCode=s.InnerCode and s.SecuCode in",tmp)
    indexPubDate <- sqlQuery(db.jy(),qr)
    indexPubDate$PubDate <- intdate2r(indexPubDate$PubDate)
    indexPubDate[indexPubDate$PubDate<as.Date('2005-01-04'),'PubDate'] <- as.Date('2005-01-04')
    
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
    
    tmpdate <- ddply(indexComp,.(indexID),summarise,mindate=intdate2r(min(InDate)))
    indexPubDate <- merge(indexPubDate,tmpdate,by='indexID')
    indexPubDate$PubDate <- as.Date(ifelse(indexPubDate$PubDate<indexPubDate$mindate,indexPubDate$mindate,indexPubDate$PubDate))
    indexPubDate <- indexPubDate[,c("indexID","indexName","PubDate")]
    
    for(i in 1:nrow(indexPubDate)){
      ptm <- proc.time()
      cat(as.character(indexPubDate$indexID[i]),rdate2int(indexPubDate$PubDate[i]),'\n')
      tmpdate <- rdate2int(getRebDates(indexPubDate$PubDate[i],Sys.Date()-100,'day'))
      tmpdate <- data.frame(date=tmpdate)
      dbWriteTable(db.local(), name="yrf_tmp", value=tmpdate, row.names = FALSE, overwrite = TRUE)                      
      qr <- paste("SELECT a.date as date, b.stockID from yrf_tmp a, amtao_tmp b
                  where b.IndexID=", QT(indexPubDate$indexID[i]), 
                  "and b.InDate<=a.date and (b.OutDate>a.date or b.OutDate IS NULL)")     
      TS <- dbGetQuery(db.local(),qr)
      TS$date <- intdate2r(TS$date)
      if(i==1){
        qr <- paste("SELECT f.trddate 'date',f.code 'stockID',f.pettm,f.pbmrq
              FROM fsfactor f where f.trddate>=",rdate2int(min(tmpdate$date)), 
              " and f.trddate<=",rdate2int(max(tmpdate$date)))
        alldata <- sqlQuery(db.quant(),qr)
        alldata$date <- intdate2r(alldata$date)
      }
      
      TSF <- merge.x(TS,alldata,by=c('date','stockID'))
      TSF <- TSF[!is.na(TSF$pettm),]
      TSF <- TSF[!is.na(TSF$pbmrq),]
      
      #pe median
      indexvalue <- ddply(TSF,.(date),summarise,value=median(pettm))
      indexvalue <- cbind(index=indexPubDate$indexID[i],indexvalue,valtype='PE',caltype='median')
      
      #pe mean
      tmp <- TSF[,c('date','stockID','pettm')]
      tmp <- tmp[tmp$pettm>0 & tmp$pettm<1000,]
      colnames(tmp) <- c('date','stockID','factorscore')
      tmp <- RFactorModel:::factor.outlier(tmp,3)
      tmp <- ddply(tmp,.(date),summarise,value=mean(factorscore))
      tmp <- cbind(index=indexPubDate$indexID[i],tmp,valtype='PE',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp)
      
      #pb median
      tmp <- ddply(TSF,.(date),summarise,value=median(pbmrq))
      tmp <- cbind(index=indexPubDate$indexID[i],tmp,valtype='PB',caltype='median')
      indexvalue <- rbind(indexvalue,tmp)
      
      #pe mean
      tmp <- TSF[,c('date','stockID','pbmrq')]
      tmp <- tmp[tmp$pbmrq>0,]
      colnames(tmp) <- c('date','stockID','factorscore')
      tmp <- RFactorModel:::factor.outlier(tmp,3)
      tmp <- ddply(tmp,.(date),summarise,value=mean(factorscore))
      tmp <- cbind(index=indexPubDate$indexID[i],tmp,valtype='PB',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp)
      
      if(i==1){
        re <- indexvalue
      }else{
        re <- rbind(re,indexvalue)
      }
      cat('cost ',(proc.time() - ptm)[3]/60,'min.\n')
    }
    re$date <- rdate2int(re$date)
    dbWriteTable(db.local(),'QT_IndexTiming',re,row.names=FALSE)
    
  }else if(type=='update'){
    begT <- dbGetQuery(db.local(),"select max(date) 'date' from QT_IndexTiming")
    begT <- intdate2r(begT$date)
    for(i in 1:length(indexset)){
      cat(indexset[i],'\n')
      RebDates <- getRebDates(begT+1,Sys.Date()-1,'day')
      TS <- getComps(indexset[i],endT = RebDates,datasrc ='ts')
      TS <- TS[!is.na(TS$stockID),]
      TSF <- gf.PE_ttm(TS)
      TSF <- TSF[!is.na(TSF$factorscore),]
      indexvalue <- ddply(TSF,.(date),summarise,value=median(factorscore))
      indexvalue <- cbind(index=indexPubDate$SecuCode[i],indexvalue,valtype='PE',caltype='median')
      
      tmp.TSF <- TSF[TSF$factorscore>0,]
      tmp.TSF <- tmp.TSF[tmp.TSF$factorscore<1000,]
      tmp.TSF <- RFactorModel:::factor.outlier(tmp.TSF,3)
      tmp.indexvalue <- ddply(tmp.TSF,.(date),summarise,value=mean(factorscore))
      tmp.indexvalue <- cbind(index=indexPubDate$SecuCode[i],tmp.indexvalue,valtype='PE',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp.indexvalue)
      
      TSF <- gf.PB_mrq(TS)
      TSF <- TSF[!is.na(TSF$factorscore),]
      TSF <- TSF[TSF$factorscore>0,]
      tmp.indexvalue <- ddply(TSF,.(date),summarise,value=median(factorscore))
      tmp.indexvalue <- cbind(index=indexPubDate$SecuCode[i],tmp.indexvalue,valtype='PB',caltype='median')
      indexvalue <- rbind(indexvalue,tmp.indexvalue)
      
      tmp.TSF <- RFactorModel:::factor.outlier(TSF,3)
      tmp.indexvalue <- ddply(tmp.TSF,.(date),summarise,value=mean(factorscore))
      tmp.indexvalue <- cbind(index=indexPubDate$SecuCode[i],tmp.indexvalue,valtype='PB',caltype='mean')
      indexvalue <- rbind(indexvalue,tmp.indexvalue)
      if(i==1){
        re <- indexvalue
      }else{
        re <- rbind(re,indexvalue)
      }
    }

    re$date <- rdate2int(re$date)
    dbWriteTable(db.local(),'QT_IndexTiming',re,overwrite=F,append=T,row.names=FALSE)
  }else{
    
  }
  
  
}




