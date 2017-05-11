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

unfroz <- MaziBox::rpt.unfroz_show()
tmpindexname <- getIndexBasicInfo(unfroz$indexID)[,1:2]
tmpindexname <- rename(tmpindexname,indexID=SecuCode,
                       indexName=SecuAbbr)
tmpindexname <- transform(tmpindexname,
                          indexID=as.character(indexID),
                          indexName=as.character(indexName))

unfroz <- transform(unfroz,proportion=percent(proportion/100,digits = 0),
                    periodrtn_stock=percent(periodrtn_stock,digits = 1),
                    stockName=stockID2name(stockID),
                    periodrtn_index=percent(periodrtn_index,1))
unfroz <- left_join(unfroz,tmpindexname,by='indexID')
unfroz <- unfroz[,c("stockID","stockName","begT","unfroz_date","proportion"              
                    ,"periodrtn_stock","indexName","periodrtn_index")]
colnames(unfroz) <- c('股票代码','股票简称','起始跟踪日','解禁日',"解禁百分比",'区间涨跌幅','所属行业指数','行业指数涨跌幅')


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
alphafund <- alphafund[!(alphafund$wind_code %in% c('377010.OF','519712.OF',
                                                    '000082.OF','002562.OF',
                                                    '960041.OF','630005.OF',
                                                    '481017.OF')),]
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
alphafund.result <- transform(alphafund.result,variable=substr(variable,2,10),
                              value=value-1)

alphafund.table <- alphafund.result %>% arrange(type,desc(value)) %>% 
  group_by(type) %>% slice(1:20) %>% ungroup()
alphafund.table <- left_join(alphafund.table,alphafund,by=c('variable'='wind_code'))
alphafund.table <- transform(alphafund.table,NAV=round(NAV,1),
                             value=percent(value,1))
alphafund.table <- cbind(1:20,alphafund.table[1:20,c("sec_name","foundedDate","NAV","value")],
                         1:20,alphafund.table[21:40,c("sec_name","foundedDate","NAV","value")])
colnames(alphafund.table) <- c('季度排名','基金简称','成立日','总规模','季度收益',
                               '年度排名','基金简称','成立日','总规模','年度收益')
