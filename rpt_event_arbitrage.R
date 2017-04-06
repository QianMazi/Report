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
colnames(midsmallsector.rtn) <- c('业绩预增','中小板指')

midsmallsector.port <- midsmallsector$port
midsmallsector.port <- midsmallsector.port %>% group_by(date) %>%
  summarise(nstock=n())
midsmallsector.port <- xts::xts(midsmallsector.port$nstock,midsmallsector.port$date)
colnames(midsmallsector.port) <- '股票数'

midsmall.rtntable <- as.data.frame(rtn.periods(midsmallsector.rtn))
midsmall.rtntable[,1] <- percent(midsmall.rtntable[,1],digits = 1)
midsmall.rtntable[,2] <- percent(midsmall.rtntable[,2],digits = 1)


