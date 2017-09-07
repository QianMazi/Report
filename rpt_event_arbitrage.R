########################### ~~ resume stock arbitrage ~~ #################################
#get resume stock arbitrage opportunity
endT <- Sys.Date()
begT <- trday.nearby(endT, by = -1)
resumeStock <- resumeArbitrage(begT,endT)
if(!is.character(resumeStock)){
  resumeStock <- transform(resumeStock,wgtinindex=scales::percent(round(wgtinindex/100,3)),
                           IndustryPct=scales::percent(round(IndustryPct,3)),
                           OldUnit=round(OldUnit,digits = 2),
                           NewUnit=round(NewUnit,digits = 2),
                           newWeight=scales::percent(round(newWeight/100,3)))
  colnames(resumeStock) <-c('基金代码','基金简称','基金类型','复牌股票简称','停牌时占指数比重',
                            '停牌日期','复牌日期','所属行业','停牌期间行业涨跌幅',
                            '停牌前场内份额','复牌后场内份额','预估最新权重') 
}


########################### ~~ stock unfrozen ~~ #################################

# unfroz <- MaziBox::rpt.unfroz_show()
# tmpindexname <- getIndexBasicInfo(unfroz$indexID)[,1:2]
# tmpindexname <- rename(tmpindexname,indexID=SecuCode,
#                        indexName=SecuAbbr)
# tmpindexname <- transform(tmpindexname,
#                           indexID=as.character(indexID),
#                           indexName=as.character(indexName))
# 
# unfroz <- transform(unfroz,proportion=percent(proportion/100,digits = 0),
#                     periodrtn_stock=percent(periodrtn_stock,digits = 1),
#                     stockName=stockID2name(stockID),
#                     periodrtn_index=percent(periodrtn_index,1))
# unfroz <- left_join(unfroz,tmpindexname,by='indexID')
# unfroz <- unfroz[,c("stockID","stockName","begT","unfroz_date","proportion"              
#                     ,"periodrtn_stock","indexName","periodrtn_index")]
# colnames(unfroz) <- c('股票代码','股票简称','起始跟踪日','解禁日',"解禁百分比",'区间涨跌幅','所属行业指数','行业指数涨跌幅')

begT <- trday.nearby(Sys.Date(),-10)
endT <- trday.nearby(Sys.Date(),10)
unfroz<-w.wset('limitingtofreeofcompanydetail',startdate=begT,enddate=endT,'sectorid=a001010100000000;search=')$Data
unfroz <- transform(unfroz,CODE=NULL,
                    unlock_date=w.asDateTime(unlock_date,asdate = TRUE),
                    percent_change=unlock_number/floatingshares_beforechange,
                    percent_beforechange=NULL,
                    issuedshares_beforechange=NULL,
                    issuedshares_afterchange=NULL,
                    floatingshares_afterchange=NULL,
                    percent_afterchange=NULL)
unfroz <- unfroz[unfroz$percent_change>0.2,]

ipodate <-w.wss(unique(unfroz$wind_code),'ipo_date')$Data
ipodate$IPO_DATE=w.asDateTime(ipodate$IPO_DATE,asdate = TRUE)
colnames(ipodate) <- c('wind_code','ipo_date')
unfroz <- left_join(unfroz,ipodate,by='wind_code')
unfroz <- unfroz[unfroz$ipo_date<begT,]
unfroz$wind_code <- stockID2stockID(unfroz$wind_code,from = 'wind',to = 'local')



temp_ <- getPeriodrtn(stockID = unique(unfroz$wind_code), begT=begT, endT=trday.nearby(Sys.Date(),-1),datasrc = 'ts')
temp_$indexID <- stockID2indexID(stockID = temp_$stockID)$indexID
temp_ <- temp_[temp_$indexID!='EINA',]
temp2_ <- data.frame(indexID=unique(temp_$indexID),periodrtn_index=c(0))
tmpindexname <- getIndexBasicInfo(temp2_$indexID)[,1:2]
tmpindexname <- rename(tmpindexname,indexID=SecuCode,
                       indexName=SecuAbbr)
tmpindexname <- transform(tmpindexname,
                          indexID=as.character(indexID),
                          indexName=as.character(indexName))
temp2_ <- left_join(temp2_,tmpindexname,by='indexID')
for( i in 1:nrow(temp2_)){
  rtn <- getIndexQuote(stocks = temp2_$indexID[i], begT = temp_$begT[1], endT = temp_$endT[1], variables = "pct_chg", datasrc = "jy")
  temp2_$periodrtn_index[i] <-  tail(cumprod((rtn$pct_chg+1)),1)-1
}
temp_ <- left_join(temp_,temp2_,by='indexID')
temp_ <- rename(temp_,wind_code=stockID)
unfroz <- left_join(unfroz,temp_[,c("wind_code","periodrtn",'indexName',"periodrtn_index")],by='wind_code')
unfroz <- transform(unfroz,unlock_number=round(unlock_number/10000,2),
                    floatingshares_beforechange=NULL,
                    percent_change=percent(percent_change,digits = 0),
                    periodrtn=percent(periodrtn,digits = 1),
                    periodrtn_index=percent(periodrtn_index,digits = 1))
unfroz_p1 <- unfroz[stringr::str_detect(unfroz$unlock_type,'定向增发'),
                    c("wind_code","sec_name","unlock_date","unlock_number","percent_change",
                      "periodrtn","indexName","periodrtn_index")]
unfroz_p1 <- unfroz_p1[unfroz_p1$unlock_date>Sys.Date(),]
unfroz_p1 <- na.omit(unfroz_p1)
colnames(unfroz_p1) <- c('股票代码','股票简称','解禁日','解禁数量',"解禁百分比",'区间涨跌幅','所属行业指数','行业指数涨跌幅')
rownames(unfroz_p1) <- NULL
unfroz_p2 <- unfroz[stringr::str_detect(unfroz$unlock_type,'首发原股东'),
                    c("wind_code","sec_name","unlock_date","unlock_number","percent_change",
                      "periodrtn","indexName","periodrtn_index","ipo_date")]
unfroz_p2 <- unfroz_p2[unfroz_p2$unlock_date>Sys.Date(),]
colnames(unfroz_p2) <- c('股票代码','股票简称','解禁日','解禁数量',"解禁百分比",'区间涨跌幅','所属行业指数','行业指数涨跌幅','上市日')
rownames(unfroz_p2) <- NULL
unfroz_p2 <- na.omit(unfroz_p2)
########################### ~~ midlle small sector forcast~~ #################################
midsmallsector <- MaziBox::rpt.EQ002_show()
midsmallsector.rtn <- midsmallsector$rtn
tmp <- getIndexQuote('EI399005',min(zoo::index(midsmallsector.rtn)),
                     max(zoo::index(midsmallsector.rtn)),
                     variables = 'pct_chg',datasrc = 'jy')
tmp <- xts::xts(tmp$pct_chg,tmp$date)
midsmallsector.rtn <- merge(midsmallsector.rtn,tmp)
midsmallsector.rtn <- zoo::na.fill(midsmallsector.rtn,0)
colnames(midsmallsector.rtn) <- c('业绩预增','中小板指')

midsmallsector.port <- midsmallsector$port
midsmallsector.port <- midsmallsector.port %>% group_by(date) %>%
  summarise(nstock=n())
midsmallsector.port <- xts::xts(midsmallsector.port$nstock,midsmallsector.port$date)
colnames(midsmallsector.port) <- '股票数'

midsmall.rtntable <- as.data.frame(rtn.periods(midsmallsector.rtn))
midsmall.rtntable[,1] <- percent(midsmall.rtntable[,1],digits = 1)
midsmall.rtntable[,2] <- percent(midsmall.rtntable[,2],digits = 1)



########################### ~~ st rename~~ #################################
st_strat_relist <- MaziBox::strategy_st_upd(st_strat_relist)
st_rtn <- st_strat_relist$rtn
st_rtn <- xts::xts(st_rtn[,-1],order.by = st_rtn[,1])
colnames(st_rtn) <- 'ST摘帽'
st_port <- st_strat_relist$port
st_port <- st_port[st_port$date==max(st_port$date),c("date","stockID","wgt")]
st_port$stockName <- stockID2name(st_port$stockID)
st_port <- st_port[,c("date","stockID","stockName","wgt")]


########################### ~~ alpha fund~~ #################################
alphafund<-w.wset('sectorconstituent',date=Sys.Date(),'sectorid=1000023322000000')[[2]]
alphafund <- transform(alphafund,CODE=NULL,date=NULL)
alphafund <- transform(alphafund,fundtype='alpha',index_code=NA)


indexEnhance<-w.wset('sectorconstituent',date=Sys.Date(),'sectorid=2001010103000000')[[2]]
indexEnhance <- transform(indexEnhance,CODE=NULL,date=NULL)
indexEnhance$fundtype <- 'enhance'
indexEnhance_data<-w.wss(indexEnhance$wind_code,'fund_trackindexcode')[[2]]
indexEnhance_data <- dplyr::rename(indexEnhance_data,wind_code=CODE,index_code=FUND_TRACKINDEXCODE)
indexEnhance <- dplyr::left_join(indexEnhance,indexEnhance_data,by='wind_code')
indexEnhance <- indexEnhance[,colnames(alphafund)]
indexEnhance <- filter(indexEnhance,index_code!='NaN',substr(wind_code,1,1)!='F')
indexEnhance$index_code <- stringr::str_c('EI',substr(indexEnhance$index_code,1,6))

alphafund <- rbind(alphafund,indexEnhance)
alphafund <- alphafund[!(stringr::str_sub(alphafund$sec_name,start = -1) %in% c('C','E','H')),]
alphafund <- alphafund[!(alphafund$wind_code %in% c('377010.OF','519712.OF',
                                                    '000082.OF','002562.OF',
                                                    '630005.OF','519212.OF',
                                                    '165531.OF','165532.OF',
                                                    '168501.OF','165532.OF',
                                                    '165531.OF')),]

begT <- Sys.Date()-365
qr <- paste("select s.SecuCode+'.OF' 'wind_code',convert(varchar,mf.TradingDay,112) 'TradingDay',
    mf.UnitNV,mf.NVRDailyGrowthRate/100 'rtn',convert(varchar,mf2.EstablishmentDate,112) 'foundedDate'
    from MF_FundNetValueRe mf,MF_FundArchives mf2,SecuMain s
    where mf.InnerCode=s.InnerCode and mf.InnerCode=mf2.InnerCode
    and s.SecuCode in",brkQT(substr(alphafund$wind_code,1,6)),
    "and mf.TradingDay>=",QT(begT),
    "order by s.SecuCode,mf.TradingDay")
re <- queryAndClose.odbc(db.jy(),qr)
re <- transform(re,wind_code=as.character(wind_code),
                TradingDay=intdate2r(TradingDay),
                foundedDate=intdate2r(foundedDate))
alphafund <- left_join(alphafund,unique(re[,c('wind_code','foundedDate')]),by='wind_code')

qr <- paste("select s.SecuCode+'.OF' 'wind_code',mf.EndDate,mf.NetAssetsValue/100000000 'NAV'
from MF_MainFinancialIndexQ mf,SecuMain s
where mf.InnerCode=s.InnerCode and s.SecuCode in",brkQT(substr(alphafund$wind_code,1,6)),
" order by s.SecuCode,mf.EndDate desc")
size <- queryAndClose.odbc(db.jy(),qr)
size <- size %>% group_by(wind_code) %>% slice(1) %>% ungroup()
alphafund <- left_join(alphafund,size[,c("wind_code","NAV")],by='wind_code')

indexrtn <- getIndexQuote(unique(indexEnhance$index_code),begT = begT,variables = c("pct_chg"),datasrc = 'jy')
colnames(indexrtn) <- c('wind_code','TradingDay','rtn')
indexrtn$foundedDate <- begT-1

re$UnitNV <- NULL
re <- rbind(re,indexrtn)

alphafund.year <- filter(re,foundedDate<begT)
alphafund.year <- reshape2::dcast(alphafund.year,TradingDay~wind_code,value.var = 'rtn')
alphafund.year <- xts::xts(alphafund.year[,-1],order.by = alphafund.year[,1])
alphafund.year <- zoo::na.fill(alphafund.year,0)
alphafund.year <- WealthIndex(alphafund.year)
alphafund.year <- melt.ts(alphafund.year)
alphafund.year$type <- 'year'
alphafund.year <- filter(alphafund.year,time==max(time))

alphafund.quarter <- filter(re,foundedDate<(Sys.Date()-90),
                            TradingDay>=(Sys.Date()-90))
alphafund.quarter <- reshape2::dcast(alphafund.quarter,TradingDay~wind_code,value.var = 'rtn')
alphafund.quarter <- xts::xts(alphafund.quarter[,-1],order.by = alphafund.quarter[,1])
alphafund.quarter <- zoo::na.fill(alphafund.quarter,0)
alphafund.quarter <- WealthIndex(alphafund.quarter)
alphafund.quarter <- melt.ts(alphafund.quarter)
alphafund.quarter$type <- 'quarter'
alphafund.quarter <- filter(alphafund.quarter,time==max(time))

alphafund.result <- rbind(alphafund.year,alphafund.quarter)
alphafund.result <- transform(alphafund.result,variable=stringr::str_replace(variable,'X',''),
                              value=value-1)
alphafund.result.index <- alphafund.result[substr(alphafund.result$variable,1,2)=='EI',]
alphafund.result.unindex <- alphafund.result[substr(alphafund.result$variable,1,2)!='EI',]
alphafund.result.unindex <- alphafund.result.unindex[alphafund.result.unindex$variable %in% alphafund[alphafund$fundtype=='alpha','wind_code'],]

alphafund.table <- alphafund.result.unindex %>% arrange(type,desc(value)) %>% 
  group_by(type) %>% slice(1:20) %>% ungroup()
alphafund.table <- left_join(alphafund.table,alphafund[,c('wind_code',"sec_name","foundedDate","NAV")],by=c('variable'='wind_code'))
alphafund.table <- transform(alphafund.table,NAV=round(NAV,1),
                             value=percent(value,1))
alphafund.table <- cbind(1:20,alphafund.table[1:20,c("sec_name","foundedDate","NAV","value")],
                         1:20,alphafund.table[21:40,c("sec_name","foundedDate","NAV","value")])
colnames(alphafund.table) <- c('季度排名','基金简称','成立日','总规模','季度收益',
                               '年度排名','基金简称','成立日','总规模','年度收益')

########################### ~~ index enhanced fund~~ #################################
alphafund.result.tmp2 <- alphafund.result[substr(alphafund.result$variable,1,2)!='EI',]
alphafund.result.tmp2 <- alphafund.result.tmp2[alphafund.result.tmp2$variable %in% alphafund[alphafund$fundtype=='enhance','wind_code'],]

enhance.table <- left_join(alphafund.result.tmp2,alphafund[,c('wind_code',"sec_name",'index_code',"foundedDate","NAV")],by=c('variable'='wind_code'))
nindex <- enhance.table %>% filter(type=='quarter') %>% group_by(index_code) %>% summarise(n=n())
nindex <- nindex %>% filter(n>=2)
enhance.table <- enhance.table[enhance.table$index_code %in% nindex$index_code,]

alphafund.result.index$time <- NULL
alphafund.result.index <- rename(alphafund.result.index,index_code=variable,indexrtn=value)


enhance.table <- left_join(enhance.table,alphafund.result.index,by=c('type','index_code'))
enhance.table <- transform(enhance.table,time=NULL,value=value-indexrtn)
enhance.table$indexrtn <- NULL

enhance.table.reshape <- reshape2::dcast(enhance.table,variable+sec_name+index_code+foundedDate+NAV~type,value.var = 'value')
enhance.table.reshape <- arrange(enhance.table.reshape,index_code,desc(quarter))

enhance.table.reshape <- transform(enhance.table.reshape,NAV=round(NAV,1),
                                   quarter=percent(quarter,1),
                                   year=percent(year,1))


enhance.table.reshape <- enhance.table.reshape[,c("variable","sec_name", "foundedDate","NAV","quarter","year")]
enhance.table.reshape$quarter[enhance.table.reshape$quarter=='NA%'] <- ''
enhance.table.reshape$year[enhance.table.reshape$year=='NA%'] <- ''

colnames(enhance.table.reshape) <- c('基金代码','基金简称','成立日','总规模','季度超额收益','年度超额收益')







