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


lcdb.update.QT_IndexTiming <- function(type=c('update','new')){
  type <- match.arg(type)
  indexset <- c('EI801003','EI000016','EI000300','EI000905','EI399005','EI399006',
                'EI000805','EI000979','EI000808','EI000819','EI000998','EI000827',
                'EI000934','EI399959','EI000933','EI000932','EI801120')
  if(type=='new'){
    tmp <- str_c(QT(str_sub(indexset,3,8)),collapse = ",")
    qr <- paste("select s.SecuCode,s.SecuAbbr,convert(varchar(8),i.PubDate,112) 'PubDate'
                from LC_IndexBasicInfo i,SecuMain s
                where i.IndexCode=s.InnerCode and s.SecuCode in (",tmp,")")
    indexPubDate <- sqlQuery(db.jy(),qr)
    indexPubDate$SecuCode <- str_c("EI",str_pad(indexPubDate$SecuCode,6,pad="0"))
    indexPubDate$PubDate <- intdate2r(indexPubDate$PubDate)
    indexPubDate[indexPubDate$PubDate<as.Date('2005-01-04'),'PubDate'] <- as.Date('2005-01-04')
    for(i in 1:nrow(indexPubDate)){
      cat(indexPubDate$SecuCode[i],rdate2int(indexPubDate$PubDate[i]),'\n')
      RebDates <- getRebDates(indexPubDate$PubDate[i],Sys.Date()-100,'day')
      TS <- getComps(indexPubDate$SecuCode[i],endT = RebDates,datasrc ='ts')
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
    dbWriteTable(db.local(),'QT_IndexTiming',re,row.names=FALSE)
    
  }else{
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
  }
  
  
}




