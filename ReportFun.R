# lcdb.update.QT_IndexTiming(type='build')
# lcdb.update.QT_IndexTiming(type='addindex',indexset = c('EI399330','EI801003'))
# lcdb.update.QT_IndexTiming(type='update')

getIV <- function(valtype=c('PE','PB'),caltype=c('median','mean'),begT=as.Date('2005-01-04'),
                  endT=Sys.Date(),plotIndex=c('EI801003','EI000300','EI000905','EI399006'),training=250){
  valtype <- match.arg(valtype)
  caltype <- match.arg(caltype)
  
  qr <- paste("select indexID,indexName,date,value from QT_IndexTiming
              where date>=",rdate2int(begT),
              " and date<=",rdate2int(endT),
              "and valtype=",QT(valtype)," and caltype=",QT(caltype))
  re <- dbGetQuery(db.local(),qr)
  re$date <- intdate2r(re$date)
  name <- paste(valtype,caltype,sep='_')
  colnames(re) <- c("indexID","indexName","date",name)
  re <- arrange(re,indexID,date)
  re[,name] <- round(re[,name],digits = 2)
  
  Nindex <- unique(re[,c('indexID','indexName')])
  for(i in 1:nrow(Nindex)){
    Data <- re[re$indexID==Nindex$indexID[i],c('date',name)]
    Datats <- xts(Data[,-1],order.by = Data[,1])
    Datats <- TTR::runPercentRank(Datats, n = training, cumulative = T, exact.multiplier = 0.5)
    Datats <-  data.frame(date=index(Datats), value=coredata(Datats))
    Datats <- Datats[(training+1):nrow(Datats),]
    Datats <- cbind(Nindex$indexID[i],Nindex$indexName[i],Datats)
    colnames(Datats) <- c("indexID","indexName","date","percentRank")
    Datats$percentRank <- round(Datats$percentRank,digits = 2)
    tmp <- Datats[nrow(Datats),]
    tmp$percentRanklastweek <- Datats[nrow(Datats)-5,'percentRank']
    tmp$value <- Data[nrow(Data),name]
    tmp$valuelastweek <- Data[nrow(Data)-5,name]
    if(i==1){
      allresult <- Datats
      newresult <- tmp
    }else{
      allresult <- rbind(allresult,Datats)
      newresult <- rbind(newresult,tmp)
    }
  }
  newresult <- arrange(newresult,desc(percentRank))
  newresult <- newresult[,c("date","indexID","indexName","value","valuelastweek",
                            "percentRank","percentRanklastweek")]
  if(valtype=='PE'){
    colnames(newresult) <- c('日期','指数代码','指数简称','动态市盈率','上周动态市盈率',
                             '市盈率百分位','上周市盈率百分位')
  }else{
    colnames(newresult) <- c('日期','指数代码','指数简称','动态市净率','上周动态市净率',
                             '市净率百分位','上周市净率百分位')
  }

  
  valuets <- re[re$indexID %in% plotIndex,c('date','indexName',name)]
  tmp <- ddply(valuets,.(indexName),summarise,mindate=min(date))
  tmp <- max(tmp$mindate)
  valuets <- valuets[valuets$date>=tmp,]
  valuets <- dcast(valuets,date~indexName,value.var = name)
  valuets <- xts(valuets[,-1],order.by = valuets[,1])
  
  percentts <- allresult[allresult$indexID %in% plotIndex,c('date','indexName','percentRank')]
  tmp <- ddply(percentts,.(indexName),summarise,mindate=min(date))
  tmp <- max(tmp$mindate)
  percentts <- percentts[percentts$date>=tmp,]
  percentts <- dcast(percentts,date~indexName,value.var = 'percentRank')
  percentts <- xts(percentts[,-1],order.by = percentts[,1])

  result <- list(new=newresult,plotvalue=valuets,plotpercent=percentts)
  return(result)
}

lcdb.update.QT_IndexTiming<- function(type=c('update','build','addindex'),indexset=NA){
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
    begT <- trday.nearby(intdate2r(begT$date),by=-1)
    endT <- trday.nearby(Sys.Date(),by=1)
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

getIFSpread <- function(IFset=c('IF','IH','IC'),begT=Sys.Date()-365){
  
  tmp <- paste(IFset,rep(c('0Y00','0Y01','0Y02','0Y03'),each=length(IFset)),sep='')
  tmp <- paste("('",paste(tmp,sep='',collapse = "','"),"')",sep = '')
  
  qr <- paste("select convert(varchar(10),t.TradingDay,112) 'date',f.ContractName,
              case when t.ContractCode LIKE 'IF%' then 'EI000300'
              when t.ContractCode LIKE 'IH%' then 'EI000016'
              else 'EI000905' end 'indexID',
              t.ClosePrice 'Close'
              from JYDB.dbo.Fut_TradingQuote t,JYDB.dbo.Fut_ContractMain f
              where t.ContractInnerCode=f.ContractInnerCode and t.ContractCode in",tmp,
              "and t.TradingDay>=",QT(rdate2int(begT)),
              "ORDER by t.TradingDay,t.ContractCode")
  IFquote <- sqlQuery(db.jy(),qr)
  IFquote$ContractName <- str_replace_all(IFquote$ContractName,'中证500指数','IC')
  IFquote$ContractName <- str_replace_all(IFquote$ContractName,'上证50指数','IH')
  
  
  qr <- paste("select 'EI'+s.SecuCode 'indexID',convert(varchar(8),q.TradingDay,112) 'date',
              q.ClosePrice 'IndexClose'
              from jydb.dbo.QT_IndexQuote q,JYDB.dbo.SecuMain s
              where q.InnerCode=s.InnerCode
              and s.SecuCode in('000016','000300','000905') and q.TradingDay>=",QT(rdate2int(begT)),
              "ORDER by s.SecuCode,q.TradingDay")
  indexPrice <- sqlQuery(db.jy(),qr)
  IFquote <- merge(IFquote,indexPrice,by=c('date','indexID'),all.x=T)
  IFquote$date <- intdate2r(IFquote$date)
  IFquote$spread <- round(IFquote$Close-IFquote$IndexClose,digits = 2)
  IFquote$spreadpct <- round(IFquote$spread/IFquote$IndexClose,digits = 2)
  
  
  #get newest spread info
  Dates <- unique(IFquote$date)
  N <- length(Dates)
  spreadinfo <- IFquote[IFquote$date==Dates[N],c('date','ContractName','Close','spread','spreadpct')]
  tmp <- IFquote[IFquote$date==Dates[N-5],c('spread','spreadpct')]
  colnames(tmp) <- c('spreadlast','spreadpctlast')
  spreadinfo <- cbind(spreadinfo,tmp)
  colnames(spreadinfo) <- c('日期','合约简称','合约收盘价','基差','基差百分比','上周基差','上周基差百分比')
  rownames(spreadinfo) <- seq(1,nrow(spreadinfo))
  #get spreadts for plot
  spreadts <- IFquote[IFquote$ContractName %in% c("IC隔季连续","IH隔季连续","IF隔季连续"),c('date','ContractName','spreadpct')]
  spreadts <- dcast(spreadts,date~ContractName,value.var = 'spreadpct')
  spreadts <- xts(spreadts[,-1],order.by = spreadts[,1])
  
  result <- list(info=spreadinfo,ts=spreadts)
  return(result)
  
}


resumeArbitrage <- function(begDate,endDate){
  
  get.resume.stock <- function(begDate,endDate,dayinterval=20){
    qr <- paste("SELECT 'EQ'+ss.SecuCode 'stockID',ss.SecuAbbr 'stockName',
                CONVERT(varchar(20),s.SuspendDate,112) 'suspendDate',
                CONVERT(varchar(20),s.ResumptionDate,112) 'resumeDate'
                from JYDB.dbo.LC_SuspendResumption s,JYDB.dbo.SecuMain ss
                where s.ResumptionDate>=",QT(begDate),
                " and s.ResumptionDate<=",QT(endDate),
                " and s.InnerCode=ss.InnerCode and ss.SecuCategory=1")
    resume.stock <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
    
    resume.stock$suspendDate <- intdate2r(resume.stock$suspendDate)
    resume.stock$resumeDate <- intdate2r(resume.stock$resumeDate)
    resume.stock$lastSuspendDay <- trday.nearby(resume.stock$resumeDate, by = 1)
    resume.stock <- resume.stock[(resume.stock$resumeDate-resume.stock$suspendDate)>dayinterval,]
    return(resume.stock)
  }
  
  get.fund.info <- function(){
    qr <- "select s.SecuCode 'fundCode',s.SecuAbbr 'fundName','EI'+s1.SecuCode 'indexCode',s1.SecuAbbr 'indexName'
    from JYDB.dbo.MF_InvestTargetCriterion i
    inner join JYDB.dbo.SecuMain s on s.InnerCode=i.InnerCode
    inner join JYDB.dbo.SecuMain s1 on s1.InnerCode=i.TracedIndexCode
    where i.InvestTarget=90 and i.IfExecuted=1 and i.MinimumInvestRatio>=0.9
    and i.InnerCode in(
    select f.InnerCode 
    from JYDB.dbo.MF_FundArchives f,JYDB.dbo.SecuMain s
    where f.Type=3 and f.InnerCode=s.InnerCode 
    and f.ListedDate is not NULL and f.ExpireDate is NULL
    and f.FundTypeCode='1101' and f.InvestmentType=7 and s.SecuCode not like '%J'
    )"

    lof.info <- sqlQuery(db.jy(),qr)
    lof.info$type <- c("LOF")
    
    sf.info <- dbReadTable(db.local(), "SF_Info")
    
    fund.info <- sf.info[,c("MCode","MName","IndexCode","IndexName")]
    fund.info$MCode <- str_sub(fund.info$MCode,start = 1,end = 6)
    fund.info$IndexCode <- str_sub(fund.info$IndexCode,start = 1,end = 6)
    fund.info$IndexCode <- str_c('EI',fund.info$IndexCode)
    fund.info$type <- c("SF")
    colnames(fund.info) <- colnames(lof.info)
    fund.info <- rbind(fund.info,lof.info)
    
    return(fund.info)
  }
  
  get.index.component <- function(stock,index,date){
    stock <- str_c("('",str_c(str_sub(stock,3,8),collapse = "','"),"')")
    index <- str_c("('",str_c(str_sub(index,3,8),collapse = "','"),"')")
    qr <- paste("select  'EI'+s1.SecuCode 'indexID',s1.SecuAbbr 'indexName',
                'EQ'+s2.SecuCode 'stockID',s2.SecuAbbr 'stockName',
                CONVERT(varchar(20),i.EndDate,112) 'enddate',
                i.Weight,CONVERT(varchar(20),i.UpdateTime,112) 'update'
                from JYDB.dbo.LC_IndexComponentsWeight i
                left join JYDB.dbo.SecuMain s1 on i.IndexCode=s1.InnerCode
                left join JYDB.dbo.SecuMain s2 on i.InnerCode=s2.InnerCode
                where i.InnerCode in (select InnerCode from JYDB.dbo.SecuMain where SecuCode in ",stock," and SecuCategory=1) 
                and i.IndexCode in (SELECT InnerCode from JYDB.dbo.SecuMain where SecuCode in ",index,"and SecuCategory=4)",
                " and i.EndDate>=",QT(date))
    index.component <- sqlQuery(db.jy(),qr)
    if(nrow(index.component)>0){
      index.component$enddate <- intdate2r(index.component$enddate)
      return(index.component)
    }else{
      print('No qualified stock in these index!')
    }
  }
  
  get.stock.industry <- function(stock){
    stock <- str_c("('",str_c(str_sub(stock,3,8),collapse = "','"),"')")
    qr <- paste("(select 'EQ'+b.SecuCode 'stockID',e.SecuCode 'sectorID',e.SecuAbbr 'sectorName'
                from JYDB.dbo.LC_ExgIndustry as a 
                inner join JYDB.dbo.SecuMain as b on a.CompanyCode = b.CompanyCode 
                inner join JYDB.dbo.CT_SystemConst as c on a.SecondIndustryCode = c.CVALUE
                inner join JYDB.dbo.LC_CorrIndexIndustry as d on c.DM = d.IndustryCode
                INNER join JYDB.dbo.SecuMain as e on d.IndexCode = e.InnerCode 
                where a.Standard = 23 and a.IfPerformed = 1 and b.SecuCode in ",stock,
                " and b.SecuCategory = 1 and c.LB = 1755 and d.IndustryStandard = 23)
                union
                (select 'EQ'+b.SecuCode 'stockID',e.SecuCode 'sectorID',e.SecuAbbr 'sectorName'
                from JYDB.dbo.LC_ExgIndustry as a 
                inner join JYDB.dbo.SecuMain as b on a.CompanyCode = b.CompanyCode 
                inner join JYDB.dbo.CT_SystemConst as c on a.FirstIndustryCode = c.CVALUE
                inner join JYDB.dbo.LC_CorrIndexIndustry as d on c.DM = d.IndustryCode
                INNER join JYDB.dbo.SecuMain as e on d.IndexCode = e.InnerCode 
                where a.Standard = 23 and a.IfPerformed = 1 and b.SecuCode in ",stock,
                " and b.SecuCategory = 1 and c.LB = 1755 and d.IndustryStandard = 23)")
    stock.industry <- sqlQuery(db.jy(),qr)
    return(stock.industry)
  }
  
  get.industry.quote <- function(industry,begday){
    industry <- str_c("('",str_c(industry,collapse = "','"),"')")
    qr <- paste("SELECT s.SecuCode 'sectorID',CONVERT(varchar(20),q.[TradingDay],112) 'date',
                q.[ClosePrice] 'close'
                FROM [JYDB].[dbo].[QT_IndexQuote] q,JYDB.dbo.SecuMain s
                where q.InnerCode=s.InnerCode and q.[TradingDay]>=",QT(begday),
                " and s.SecuCategory=4 and s.SecuCode in ",industry,
                " order by s.SecuCode,q.TradingDay")
    index.quote <- sqlQuery(db.jy(),qr)
    index.quote$date <- intdate2r(index.quote$date)
    return(index.quote)
  }
  
  calc.match.industrypct <- function(resume.stock,index.quote){
    colnames(index.quote) <- c("sectorID","suspendDate","close1" )
    resume.stock <- merge(resume.stock,index.quote,by=c('sectorID','suspendDate'),all.x = T)
    colnames(index.quote) <- c("sectorID","lastSuspendDay","close2" )
    resume.stock <- merge(resume.stock,index.quote,by=c('sectorID','lastSuspendDay'),all.x = T)
    resume.stock$IndustryPct <- resume.stock$close2/resume.stock$close1-1
    resume.stock <- resume.stock[abs(resume.stock$IndustryPct)>=0.1,]
    
    if(nrow(resume.stock)>0){
      resume.stock <- resume.stock[,c("stockID","stockName","suspendDate", "resumeDate",    
                                      "lastSuspendDay","sectorID","sectorName","IndustryPct")]
      return(resume.stock)
    }else{
      print('No valuation adjustment!')
    }
    
    
  }
  
  calc.match.indexcomponent <- function(resume.stock,index.component,bar=2){
    result <- data.frame()
    for(i in 1:nrow(resume.stock)){
      tmp.result <- resume.stock[i,]
      tmp.index.component <- index.component[index.component$stockID==resume.stock$stockID[i],]
      if(length(unique(tmp.index.component$indexID))>1){
        for(j in unique(tmp.index.component$indexID)){
          tmp <- tmp.index.component[tmp.index.component$indexID==j,]
          tmp <- arrange(tmp,enddate)
          ind <- findInterval(resume.stock[i,"suspendDate"],tmp$enddate)
          if(ind==0 || max(tmp$enddate)<resume.stock[i,"suspendDate"]) next
          tmp.result$inindex <- tmp$indexID[ind]
          tmp.result$inindexname <- tmp$indexName[ind]
          tmp.result$wgtinindex <- tmp$Weight[ind]
          result <- rbind(result,tmp.result)
        }
      }else{
        tmp <- tmp.index.component
        tmp <- arrange(tmp,enddate)
        ind <- findInterval(resume.stock[i,"suspendDate"],tmp$enddate)
        if(ind==0 || max(tmp$enddate)<resume.stock[i,"suspendDate"]) next
        tmp.result$inindex <- tmp$indexID[ind]
        tmp.result$inindexname <- tmp$indexName[ind]
        tmp.result$wgtinindex <- tmp$Weight[ind]
        result <- rbind(result,tmp.result)
      }
    }
    result <- result[result$wgtinindex>=bar,]
    if(nrow(result)>0) return(result)
    else print("No qualified index!")
  }
  
  calc.match.fundunit <- function(fund.result){
    tmp.sfcode <- unique(fund.result$fundCode[fund.result$type=='SF'])
    tmp.sfcode <- str_c(tmp.sfcode,'.OF')
    tmp.lofcode <- unique(fund.result$fundCode[fund.result$type=='LOF'])
    tmp.begDate <- min(fund.result$suspendDate)
    fund.size <- data.frame()
    if(length(tmp.sfcode)>0){
      tmp <- str_c("('",str_c(tmp.sfcode,collapse = "','"),"')")
      qr <- paste("select t.MCode,i.MName,t.Date,
                  (t.MUnit*t.MNav+ t.AUnit*t.ANav + t.BUnit*t.BNav) 'Unit'
                  from SF_TimeSeries t,SF_Info i
                  where t.MCode=i.MCode and t.MCode in",tmp,
                  "and t.Date>=",rdate2int(tmp.begDate))
      
      sf.size <- dbGetQuery(db.local(),qr)
      colnames(sf.size) <- c("Code","Name","Date","Unit")
      sf.size$Code <- str_sub(sf.size$Code,start = 1,end = 6)
      fund.size <-rbind(fund.size,sf.size) 
    }
    
    if(length(tmp.lofcode)>0){
      tmp <- str_c("('",str_c(tmp.lofcode,collapse = "','"),"')")
      qr <- paste("select s.SecuCode,s.SecuAbbr,CONVERT(varchar(20),m.EndDate,112) 'EndDate',m.FloatShares/100000000 'Unit'
                  from JYDB.dbo.MF_SharesChange m,JYDB.dbo.SecuMain s
                  where m.InnerCode=s.InnerCode and s.SecuCode in",tmp,
                  " and m.StatPeriod='996' and m.EndDate>=",
                  QT(tmp.begDate),
                  " order by s.SecuCode,m.EndDate")
      
      lof.size <- sqlQuery(db.jy(),qr)
      colnames(lof.size) <- c("Code","Name","Date","Unit")
      fund.size <-rbind(fund.size,lof.size) 
    }
    fund.size$Date <- intdate2r(fund.size$Date)
    
    fund.result$OldUnit <- c(0)
    fund.result$NewUnit <- c(0)
    for(i in 1:nrow(fund.result)){
      tmp <- fund.size[fund.size$Code==fund.result$fundCode[i] & !is.na(fund.size$Unit),]
      tmp <- arrange(tmp,Date)
      if(tmp$Date[1]>fund.result$suspendDate[i]) fund.result$OldUnit[i] <- tmp$Unit[1]
      else fund.result$OldUnit[i] <- tmp$Unit[tmp$Date==fund.result$suspendDate[i]]
      fund.result$NewUnit[i] <- tmp$Unit[nrow(tmp)]
    }
    
    fund.result$UnitPct <- as.numeric(fund.result$NewUnit)/as.numeric(fund.result$OldUnit)-1  
    fund.result$newWeight <- fund.result$wgtinindex*fund.result$OldUnit/fund.result$NewUnit
    fund.result <- fund.result[fund.result$newWeight>=5 & fund.result$NewUnit>0.5,]
    if(nrow(fund.result)>0){
      fund.result <- fund.result[,c("fundCode","fundName","type",      
                                    "stockName","wgtinindex","suspendDate","resumeDate",
                                    "sectorName","IndustryPct","OldUnit","NewUnit",'newWeight')]
      fund.result <- arrange(fund.result,desc(newWeight))
      return(fund.result)
    }else{
      print("No qualified stock!")
    }
    
  }
  
  
  resume.stock <- get.resume.stock(begDate,endDate) #get qualified resumption stock
  if(nrow(resume.stock)==0) return("None!")
  
  fund.info <- get.fund.info() #get all lof and structure fund basic info
  
  #get resumption stock in the traced index of these lof and sf
  tmp.index <- str_to_upper(unique(fund.info$indexCode))
  tmp.date <- trday.offset(min(resume.stock$suspendDate), by = months(-1))
  index.component <- get.index.component(resume.stock$stockID,tmp.index,tmp.date)
  if(is.character(index.component)) return("None!")
  
  #get stock's amac industy and the industry's corresponding index quote
  resume.stock <- resume.stock[resume.stock$stockID %in% index.component$stockID,]
  stock.industry <- get.stock.industry(resume.stock$stockID)
  resume.stock <- merge(resume.stock,stock.industry,by='stockID')
  index.quote <- get.industry.quote(industry = resume.stock$sectorID,begday = min(resume.stock$suspendDate))
  
  #calculate stock's industry change pct during suspend time
  resume.stock <- calc.match.industrypct(resume.stock,index.quote)
  if(is.character(resume.stock)) return("None!")
  #
  resume.stock <- calc.match.indexcomponent(resume.stock,index.component)
  if(is.character(resume.stock)) return("None!")
  
  
  # get the final result
  fund.info <- fund.info[fund.info$indexCode %in% resume.stock$inindex,]
  fund.result <- merge(fund.info,resume.stock,by.x='indexCode',by.y = 'inindex',all.x =T)
  fund.result <- arrange(fund.result,fundCode)
  fund.result <- fund.result[,c("fundCode","fundName","indexCode","indexName","type","stockID","stockName",      
                                "wgtinindex","suspendDate","resumeDate","lastSuspendDay",
                                "sectorID","sectorName","IndustryPct")]
  fund.result <- calc.match.fundunit(fund.result)
  
  return(fund.result)
  
}


getMASeries <- function(){
  qr <- "select 'EI'+s.SecuCode 'indexID',s.SecuAbbr 'indexName',
  c.DM 'industryCode',c.MS 'industryName'
  from jydb.dbo.LC_CorrIndexIndustry l,JYDB.dbo.SecuMain s,
  jydb.dbo.CT_SystemConst c
  where l.IndustryStandard=24 
  and l.IndexCode=s.InnerCode and l.IndustryCode=c.DM
  and c.LB=1804 and c.IVALUE=1"
  indexInd <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
  indexInd <- indexInd[!str_detect(indexInd$indexName,'三板'),]
  
  tmp <- str_c("('",str_c(str_sub(indexInd$indexID,3,8),collapse = "','"),"')")
  qr <- paste("select CONVERT(varchar(20),q.[TradingDay],112) 'date',
              'EI'+s.SecuCode 'indexID',q.ClosePrice 'close'
              from [JYDB].[dbo].[QT_IndexQuote] q,JYDB.dbo.SecuMain s
              where q.InnerCode=s.InnerCode and s.SecuCode in",tmp,
              " and q.TradingDay>=",QT(Sys.Date()-730),
              " ORDER by s.SecuCode,q.TradingDay")
  indexQuote <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
  indexQuote$date <- intdate2r(indexQuote$date)
  for(i in 1:nrow(indexInd)){
    tmp <- indexQuote[indexQuote$indexID==indexInd$indexID[i],]
    tmp <- transform(tmp,MA1=SMA(close,8),MA2=SMA(close,13),MA3=SMA(close,21),MA4=SMA(close,34),
                     MA5=SMA(close,55),MA6=SMA(close,89),MA7=SMA(close,144),MA8=SMA(close,233))
    tmp <- tmp[!is.na(tmp$MA8),]
    tmp$score <- (tmp$close>tmp$MA1)+(tmp$close>tmp$MA2)+(tmp$close>tmp$MA3)+(tmp$close>tmp$MA4)+
      (tmp$close>tmp$MA5)+(tmp$close>tmp$MA6)+(tmp$close>tmp$MA7)+(tmp$close>tmp$MA8)
    tmp <- tmp[,c('date','indexID','score')]
    if(i==1){
      indexScore <- tmp
    }else{
      indexScore <- rbind(indexScore,tmp)
    }
  }
  dates <- unique(indexScore$date)
  N <- length(dates)
  dates <- dates[c(N-5,N)]
  new <- indexScore[indexScore$date %in% dates,]
  new <- dcast(new,indexID~date,value.var = 'score')
  colnames(new) <- c("indexID","lastweek","today")
  new$date <- dates[2]
  new <- merge(new,indexInd[,c('indexID','indexName')],by='indexID')
  new <- arrange(new,desc(today))
  new <- new[,c("date","indexID","indexName","today","lastweek")]
  colnames(new) <- c('日期','指数代码','指数名称','最新值','上周值')
  
  indexScore <- dcast(indexScore,date~indexID,value.var = 'score')
  indexScore$score <- round(rowSums(indexScore[,-1])/nrow(indexInd),digits = 1)
  indexScore <- indexScore[,c('date','score')]
  indexScore <- xts(indexScore[,-1],order.by = indexScore[,1])
  names(indexScore) <- c('大盘均线强弱指数')
  result <- list(ts=indexScore,data=new)
  return(result)
}


LLT <- function(indexID='EI000300',d=60,trancost=0.001,type=c('LLT','SMA')){
  type <- match.arg(type)
  
  qr <- paste("select CONVERT(varchar(20),q.[TradingDay],112) 'date',
              'EI'+s.SecuCode 'indexID',q.OpenPrice 'open',q.ClosePrice 'close',q.ChangePCT/100 'indexRtn'
              from [JYDB].[dbo].[QT_IndexQuote] q,JYDB.dbo.SecuMain s
              where q.InnerCode=s.InnerCode and s.SecuCode=",QT(str_sub(indexID,3,8)),
              " and q.TradingDay>='2010-01-04' ORDER by q.TradingDay")
  indexQuote <- sqlQuery(db.jy(),qr,stringsAsFactors=F)
  indexQuote$date <- intdate2r(indexQuote$date)
  
  if(type=='LLT'){
    alpha <- 2/(1+d)
    
    indexQuote$LLT <- c(0)
    indexQuote$LLT[1] <- indexQuote$close[1]
    indexQuote$LLT[2] <- indexQuote$close[2]
    for(i in 3:nrow(indexQuote)){
      indexQuote$LLT[i] <- (alpha-alpha^2/4)* indexQuote$close[i]+
        (alpha^2/2)*indexQuote$close[i-1]-(alpha-3*alpha^2/4)*indexQuote$close[i-2]+
        2*(1-alpha)*indexQuote$LLT[i-1]-(1-alpha)^2*indexQuote$LLT[i-2]
    }
    indexQuote <- indexQuote[d:nrow(indexQuote),]
    rownames(indexQuote) <- seq(1,nrow(indexQuote))
    
    indexQuote$pos <- c(0)
    indexQuote$signal <- c('')
    indexQuote$tmp <- indexQuote$indexRtn
    for(i in 3:nrow(indexQuote)){
      if(indexQuote$LLT[i-1]>indexQuote$LLT[i-2] && indexQuote$pos[i-1]==0){
        indexQuote$pos[i] <- 1
        indexQuote$tmp[i] <- indexQuote$close[i]/indexQuote$open[i]-1-trancost
      }else if(indexQuote$LLT[i-1]>indexQuote$LLT[i-2] && indexQuote$pos[i-1]==1){
        indexQuote$pos[i] <- 1
      }else if(indexQuote$LLT[i-1]<indexQuote$LLT[i-2] && indexQuote$pos[i-1]==1){
        indexQuote$pos[i] <- 0
        indexQuote$tmp[i] <- indexQuote$open[i]/indexQuote$close[i-1]-1-trancost
      }
      
      if(indexQuote$LLT[i]>indexQuote$LLT[i-1] && indexQuote$pos[i]==0){
        indexQuote$signal[i] <- 'buy'
      }else if(indexQuote$LLT[i]>indexQuote$LLT[i-1] && indexQuote$pos[i]==1){
        indexQuote$signal[i] <- 'hold'
      }else if(indexQuote$LLT[i]<indexQuote$LLT[i-1] && indexQuote$pos[i]==1){
        indexQuote$signal[i] <- 'sell'
      }
      
      
      
    }
    
  }else{
    indexQuote$MA <- SMA(indexQuote$close,d)
    indexQuote <- indexQuote[d:nrow(indexQuote),]
    rownames(indexQuote) <- seq(1,nrow(indexQuote))
    
    indexQuote$pos <- c(0)
    indexQuote$signal <- c('')
    indexQuote$tmp <- indexQuote$indexRtn
    for(i in 3:nrow(indexQuote)){
      if(indexQuote$close[i-1]>indexQuote$MA[i-1] && indexQuote$pos[i-1]==0){
        indexQuote$pos[i] <- 1
        indexQuote$tmp[i] <- indexQuote$close[i]/indexQuote$open[i]-1-trancost
      }else if(indexQuote$close[i-1]>indexQuote$MA[i-1] && indexQuote$pos[i-1]==1){
        indexQuote$pos[i] <- 1
      }else if(indexQuote$close[i-1]<indexQuote$MA[i-1] && indexQuote$pos[i-1]==1){
        indexQuote$pos[i] <- 0
        indexQuote$tmp[i] <- indexQuote$open[i]/indexQuote$close[i-1]-1-trancost
      }
      
      if(indexQuote$close[i]>indexQuote$MA[i] && indexQuote$pos[i]==0){
        indexQuote$signal[i] <- 'buy'
      }else if(indexQuote$close[i]>indexQuote$MA[i] && indexQuote$pos[i]==1){
        indexQuote$signal[i] <- 'hold'
      }else if(indexQuote$close[i]<indexQuote$MA[i] && indexQuote$pos[i]==1){
        indexQuote$signal[i] <- 'sell'
      }
      
      
    }
    
    
    
  }
  
  indexQuote$strRtn <- indexQuote$tmp*indexQuote$pos
  indexQuote <- subset(indexQuote,select=-c(pos,tmp))
  rtn <- xts(indexQuote[,c('indexRtn','strRtn')],order.by = indexQuote$date)
  result <- list(rawdata=indexQuote,rtn=rtn)
  return(result)
  
}



gridTrade.IF <- function(indexID='EI000300',begT=as.Date('2015-09-01'),endT=Sys.Date(),para){
  
  getData <- function(indexID,begT,endT){
    if(indexID=='EI000300'){
      tmp <- 'IF1%'
    }else if(indexID=='EI000905'){
      tmp <- 'IC1%'
    }else if(indexID=='EI000016'){
      tmp <- 'IH1%'
    }
    
    #get index future quote
    qr <- paste("select convert(varchar(10),t.TradingDay,112) 'date',
                t.ContractCode 'stockID',
                convert(varchar(10),c.EffectiveDate,112) 'effectiveDate',
                convert(varchar(10),c.LastTradingDate,112) 'lastTradingDate',
                t.ClosePrice 'close'
                from JYDB.dbo.Fut_TradingQuote t,JYDB.dbo.Fut_ContractMain c 
                where t.ContractInnerCode=c.ContractInnerCode and
                t.ContractCode like ",QT(tmp), 
                " and t.TradingDay>=",QT(begT)," and t.TradingDay<=",QT(endT),
                " order by t.TradingDay,t.ContractCode")
    indexData <- sqlQuery(db.jy(),qr,stringsAsFactors =F)
    indexData <- transform(indexData,date=intdate2r(date),effectiveDate=intdate2r(effectiveDate),
                           lastTradingDate=intdate2r(lastTradingDate))
    indexData$lastTradingDate <- trday.nearby(indexData$lastTradingDate,by=1)
    indexData.copy <- indexData
    
    # keep the next quarter contract
    indexData$tmp <- c(0)
    shiftData <- data.frame()
    for(i in 1:nrow(indexData)){
      if(i==1){
        IF.ID <- indexData$stockID[4]
        IF.lastDay <- indexData$lastTradingDate[4]
      }
      if(indexData$stockID[i]==IF.ID && indexData$date[i]<IF.lastDay){
        indexData$tmp[i] <- 1
      }else if(indexData$stockID[i]==IF.ID && indexData$date[i]==IF.lastDay){
        shiftData <- rbind(shiftData,indexData[i,c("date","stockID","close")])
        tmp <- indexData[indexData$date==indexData$date[i],]
        IF.ID <- tmp$stockID[4]
        IF.lastDay <- tmp$lastTradingDate[4]
      }
    }
    indexData <- indexData[indexData$tmp==1,c("date","stockID","close")]
    
    #get index quote
    qr <- paste("select convert(varchar(10),q.TradingDay,112) 'date',
                q.ClosePrice 'benchClose'
                from jydb.dbo.QT_IndexQuote q,JYDB.dbo.SecuMain s
                where q.InnerCode=s.InnerCode and s.SecuCode=",QT(substr(indexID,3,8)),
                "and q.TradingDay>=",QT(begT)," and q.TradingDay<=",QT(endT),
                "order by q.TradingDay")
    tmp <- sqlQuery(db.jy(),qr)
    tmp$date <- intdate2r(tmp$date)
    indexData <- merge(indexData,tmp,by='date',all.x=T)
    alldata <- list(indexData=indexData,shiftData=shiftData)
    return(alldata)
  }  
  
  calcData <- function(indexData,shiftData,para){
    indexData <- transform(indexData,benchPct=round(benchClose/indexData$benchClose[1]-1,digits = 3),
                           pos=c(0),mv=c(0),cost=c(0),cash=c(0),totalasset=c(0),remark=NA)
    for(i in 1:nrow(indexData)){
      #initial
      if(i==1){
        indexData$pos[i] <-para$initPos 
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
        indexData$cost[i] <-para$tradeCost*indexData$mv[i]
        indexData$cash[i] <-para$total-indexData$mv[i]-indexData$cost[i]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        indexData$remark[i] <-'initial'
        next
      }
      
      # shift positions
      if(indexData$stockID[i]!=indexData$stockID[i-1]){
        tmp <- subset(shiftData,stockID==indexData$stockID[i-1] & date==indexData$date[i],select=close)
        indexData$pos[i] <-indexData$pos[i-1]
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
        indexData$cost[i] <-para$tradeCost*(indexData$mv[i]+indexData$pos[i-1]*tmp$close*para$multiplier)
        indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]+indexData$pos[i-1]*tmp$close*para$multiplier-indexData$mv[i]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        indexData$remark[i] <-'shift position'
      }
      
      #position change
      if(indexData$benchPct[i]>para$bar){
        todayPos <- para$initPos-floor(indexData$benchPct[i]/para$bar)
      }else if(indexData$benchPct[i]<(-1*para$bar)){
        todayPos <- para$initPos+floor(abs(indexData$benchPct[i]/para$bar))
      }else{
        todayPos <- para$initPos
      }
      
      if(todayPos<indexData$pos[i-1] & indexData$pos[i-1]>0){
        posChg <- min(indexData$pos[i-1]-todayPos,indexData$pos[i-1])
        #subtract position
        if(is.na(indexData$remark[i])){
          indexData$pos[i] <-indexData$pos[i-1]-posChg
          indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
          indexData$cost[i] <-para$tradeCost*posChg*para$multiplier*indexData$close[i]
          indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]+posChg*para$multiplier*indexData$close[i]
          indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
          indexData$remark[i] <-'subtract position'
        }else{
          #shift position + subtract position
          indexData$pos[i] <-indexData$pos[i]-posChg
          indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
          indexData$cost[i] <-indexData$cost[i]+para$tradeCost*posChg*para$multiplier*indexData$close[i]
          indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]+posChg*para$multiplier*indexData$close[i]
          indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        }
      }else if(todayPos>indexData$pos[i-1] & indexData$cash[i-1]>=indexData$close[i]*para$multiplier){
        #add position
        posChg <- min(todayPos-indexData$pos[i-1],floor(indexData$cash[i-1]/indexData$close[i]*para$multiplier))
        if(is.na(indexData$remark[i])){
          indexData$pos[i] <-indexData$pos[i-1]+posChg
          indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
          indexData$cost[i] <-para$tradeCost*posChg*para$multiplier*indexData$close[i]
          indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]-posChg*para$multiplier*indexData$close[i]
          indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
          indexData$remark[i] <-'add position'
        }else{
          #shift position + add position
          indexData$pos[i] <-indexData$pos[i]+posChg
          indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
          indexData$cost[i] <-indexData$cost[i]+para$tradeCost*posChg*para$multiplier*indexData$close[i]
          indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]-posChg*para$multiplier*indexData$close[i]
          indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        }
        
      }else{
        #hold position
        if(is.na(indexData$remark[i])){
          indexData$pos[i] <-indexData$pos[i-1]
          indexData$mv[i] <-indexData$pos[i]*indexData$close[i]*para$multiplier
          indexData$cash[i] <-indexData$cash[i-1]
          indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        }else next
      }
    }
    
    return(indexData)
  }
  
  allData <- getData(indexID,begT,endT)
  indexData <- allData$indexData
  shiftData <- allData$shiftData
  
  indexData <- calcData(indexData,shiftData,para)
  indexData <- indexData[,c("date","stockID","close","benchClose","benchPct","pos","mv","totalasset","remark" )]
  return(indexData)
}



gridTrade.index <- function(indexID='EI000300',fundID='SH510500',begT=as.Date('2015-09-01'),endT=Sys.Date(),para){
  getData <- function(indexID,fundID,begT,endT){
    #get index quote
    stocks <- c(fundID,indexID) 
    indexData <- getQuote_ts(stocks, begT, endT, 'close', cycle = "cy_day()", rate = 1,rateday = 0)
    indexData <- dcast(indexData,date~stockID,value.var = 'close')
    indexData$stockID <- fundID
    indexData <- indexData[,c('date','stockID',stocks)]
    colnames(indexData) <- c('date','stockID','close','benchClose')
    return(indexData)
  }  
  
  calcData <- function(indexData,para){
    indexData <- transform(indexData,benchPct=round(benchClose/indexData$benchClose[1]-1,digits = 3),
                           pos=c(0),mv=c(0),invest=c(0),cost=c(0),cash=c(0),totalasset=c(0),remark=NA)
    for(i in 1:nrow(indexData)){
      #initial
      if(i==1){
        indexData$invest[i] <- para$initmv
        indexData$pos[i] <- floor(para$initmv/(indexData$close[i]*100))*100
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]
        indexData$cost[i] <-indexData$mv[i]*para$tradeCost
        indexData$cash[i] <-para$total-indexData$mv[i]-indexData$cost[i]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        indexData$remark[i] <-'initial'
        next
      }
      
      #position change
      if(indexData$benchPct[i]>para$bar){
        todayInvest <- para$initmv-floor(indexData$benchPct[i]/para$bar)*para$mvChg
      }else if(indexData$benchPct[i]<(-1*para$bar)){
        todayInvest <- para$initmv+floor(abs(indexData$benchPct[i]/para$bar))*para$mvChg
      }else{
        todayInvest <- para$initmv
      }
      
      if(todayInvest<indexData$invest[i-1] & indexData$mv[i-1]>0){
        #subtract position
        investChg <- min(indexData$invest[i-1]-todayInvest,indexData$mv[i-1])
        indexData$invest[i] <- max(todayInvest,0)
        chgPos <- floor(investChg/(indexData$close[i]*100))*100
        indexData$pos[i] <-indexData$pos[i-1]-chgPos
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]
        indexData$cost[i] <-chgPos*indexData$close[i]*para$tradeCost
        indexData$cash[i] <-indexData$cash[i-1]-indexData$cost[i]+chgPos*indexData$close[i]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        indexData$remark[i] <-'subtract position'
        
      }else if(todayInvest>indexData$invest[i-1] & indexData$cash[i-1]>0){
        #add position
        investChg <- min(todayInvest-indexData$invest[i-1],indexData$cash[i-1])
        indexData$invest[i] <- min(todayInvest,para$total)
        chgPos <- floor(investChg/(indexData$close[i]*100))*100
        indexData$pos[i] <-indexData$pos[i-1]+chgPos
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]
        indexData$cost[i] <-chgPos*indexData$close[i]*para$tradeCost
        indexData$cash[i] <-indexData$cash[i-1]-chgPos*indexData$close[i]-indexData$cost[i]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        indexData$remark[i] <-'add position'
        
      }else{
        #hold position
        indexData$invest[i] <- indexData$invest[i-1]
        indexData$pos[i] <-indexData$pos[i-1]
        indexData$mv[i] <-indexData$pos[i]*indexData$close[i]
        indexData$cash[i] <-indexData$cash[i-1]
        indexData$totalasset[i] <-indexData$mv[i]+indexData$cash[i]
        
      }
    }
    return(indexData)
  }
  
  indexData <- getData(indexID,fundID,begT,endT)
  
  indexData <- calcData(indexData,para)
  indexData <- indexData[,c("date","stockID","close","benchClose","benchPct","pos","mv","totalasset","remark")]
  return(indexData)
}



