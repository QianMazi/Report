library(quantbox)
library(WindR)
w.start(showmenu = F)
tsInclude()
tsConnect()

########################### ~~ index valuation ~~ #################################
cat('caculating index valuation ...\n')
#update index valuation data
lcdb.update.QT_IndexTiming()

#get index valuation result
indexValue <- getIV(valtype = 'PE')
tmp <- getIV(valtype = 'PB',caltype = 'median')

tablevaluation <- merge(indexValue,tmp,by=c("date","indexID","indexName"))
tablevaluation <- tablevaluation[tablevaluation$date==max(tablevaluation$date),]
colnames(tablevaluation) <- c("date","indexID","indexName","PE_median","perPE","PB_median","perPB")
tablevaluation <- arrange(tablevaluation,desc(perPE))
tablevaluation <- transform(tablevaluation,
                            PE_median=round(PE_median,1),
                            perPE=scales::percent(perPE),
                            PB_median=round(PB_median,1),
                            perPB=scales::percent(perPB))
colnames(tablevaluation) <- c('日期','指数代码','指数简称','动态市盈率','市盈率百分位','动态市净率','市净率百分位')

valuets <- merge(indexValue,tmp,by=c("date","indexID","indexName"))
valuets <- valuets[valuets$indexID %in% c('EI801003','EI000300','EI399006'),]
colnames(valuets) <- c("date","indexID","indexName","PE_median","perPE","PB_median","perPB")
valuets <- valuets[valuets$date>=(max(valuets$date)-months(18)),]

#special valuation
endT <- Sys.Date()-1
load('specialvalue.RData')
begT <- max(zoo::index(bond_yield))
begT <- w.tdaysoffset(1,begT)[[2]]
begT <- begT$DATETIME
endT <- (w.tdaysoffset(-1,Sys.Date())[[2]])$DATETIME
if(begT<=endT){
  tmp.bond_yield<-w.edb('M1000166',begT,endT,'Fill=Previous')[[2]]
  tmp.bond_yield <- xts::xts(tmp.bond_yield[,-1],order.by = tmp.bond_yield[,1])
  colnames(tmp.bond_yield) <- 'bond10Y'
  bond_yield <- rbind(bond_yield,tmp.bond_yield)
  
}

begT <- max(zoo::index(commodity_price))
begT <- w.tdaysoffset(1,begT,"TradingCalendar=CBOT")[[2]]
begT <- begT$DATETIME
endT <- (w.tdaysoffset(-1,Sys.Date(),"TradingCalendar=CBOT")[[2]])$DATETIME
if(begT<=endT){
  gold<-w.wsd("GC00.CMX","close",begT,endT,"TradingCalendar=CBOT;Fill=Previous")[[2]]
  silver<-w.wsd("SI00.CMX","close",begT,endT,"TradingCalendar=CBOT;Fill=Previous")[[2]]
  oil<-w.wsd("CL00.NYM","close",begT,endT,"TradingCalendar=NYMEX;Fill=Previous")[[2]]
  colnames(gold) <- c('DATETIME','GC00.CMX')
  colnames(silver) <- c('DATETIME','SI00.CMX')
  colnames(oil) <- c('DATETIME','CL00.NYM')
  tmp.commodity_price <- dplyr::full_join(gold,silver,by='DATETIME')
  tmp.commodity_price <- dplyr::full_join(tmp.commodity_price,oil,by='DATETIME')
  tmp.commodity_price <- dplyr::arrange(tmp.commodity_price,DATETIME)
  tmp.commodity_price <- xts::xts(tmp.commodity_price[,-1],order.by = tmp.commodity_price[,1])
  commodity_price <- rbind(commodity_price,tmp.commodity_price)
  commodity_price <- zoo::na.locf(commodity_price)
  
}
save(bond_yield,commodity_price,file='specialvalue.RData')

value.bond_yield <- TTR::runPercentRank(bond_yield, n = 250, cumulative = T, exact.multiplier = 0.5)
colnames(value.bond_yield) <- '10年期国债收益率'
value.bond_yield <- value.bond_yield['2015/']
commodity_price$goldsilver <- commodity_price[,'GC00.CMX']/commodity_price[,'SI00.CMX']
commodity_price$goldoil <- commodity_price[,'GC00.CMX']/commodity_price[,'CL00.NYM']

value.tmp1 <- TTR::runPercentRank(commodity_price[,'goldsilver'], n = 250, cumulative = T, exact.multiplier = 0.5)
value.tmp2 <- TTR::runPercentRank(commodity_price[,'goldoil'], n = 250, cumulative = T, exact.multiplier = 0.5)
value.commodity_price <-  merge(value.tmp1,value.tmp2)
colnames(value.commodity_price) <- c('金银比','金油比')
value.commodity_price <- value.commodity_price['2015/']

value.all <- merge(value.bond_yield,value.commodity_price)
value.all <- zoo::na.locf(value.all)
value.all <- na.omit(value.all)


########################### ~~ index futures spread ~~ #################################
cat('caculating index futures spread ...\n')
IFSpread <- getIFSpread(begT = Sys.Date()-180)
#get spreadts for plot
tmp <- data.frame(stockIDCon=c('IC0Y00','IC0Y01','IC0Y02','IC0Y03','IF0Y00','IF0Y01','IF0Y02','IF0Y03','IH0Y00','IH0Y01','IH0Y02','IH0Y03'),
                  ConName=c('IC当月','IC次月','IC当季','IC次季','IF当月','IF次月','IF当季','IF次季','IH当月','IH次月','IH当季','IH次季'))
spreadts <- IFSpread[,c("date","stockIDCon","spreadPct","spreadPctAna")]
spreadts <- merge.x(spreadts,tmp)
spreadts <- transform(spreadts,spreadPct=spreadPct*100,
                      spreadPctAna=spreadPctAna*100)

spreadinfo <- IFSpread[IFSpread$date==max(IFSpread$date),c("date","stockID","close","spread","spreadPct","spreadPctAna")]
spreadinfo <- transform(spreadinfo,
                            indexClose=round(close-spread,2),
                            spread=round(spread,2),
                            spreadPct=scales::percent(spreadPct),
                            spreadPctAna=scales::percent(spreadPctAna))
rownames(spreadinfo) <- NULL
spreadinfo <- spreadinfo[,c("date","stockID","close","indexClose","spread","spreadPct","spreadPctAna")]
colnames(spreadinfo) <- c('日期','合约代码','合约收盘价','指数收盘价','基差','基差百分比','年化基差百分比')




########################### ~~ resume stock arbitrage ~~ #################################
cat('caculating resume stock arbitrage ...\n')
#get resume stock arbitrage opportunity
endT <- Sys.Date()
begT <- trday.nearby(endT, by = 1)
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


########################### ~~ private stock offering fund ~~ #################################
cat('caculating private stock offering fund ...\n')
# private stock offering fund
privateOffering <- read.csv("privateOffering.csv", stringsAsFactors=FALSE)
privateOffering[,4] <- round(privateOffering[,4],2)
privateOffering[,5] <- round(privateOffering[,5],2)
privateOffering[,6] <- scales::percent(round(privateOffering[,6],3))
privateOffering[,7] <- scales::percent(round(privateOffering[,7],3))
privateOffering[,8] <- scales::percent(round(privateOffering[,8],3))

fundID <- substr(privateOffering[,1],1,6)
endT <- Sys.Date()
begT <- trday.offset(endT,by = months(-3))
pofund <- POFund(fundID,begT,endT)
pofund$pre <- pofund$pre*100


########################### ~~ stock unfrozen ~~ #################################
cat('caculating stock unfrozen ...\n')
lcdb.update.QT_DailyQuote2()
unfroz <- MaziBox::rpt.unfroz_show()
tmpindexname <- getIndexBasicInfo(unfroz$indexID)
tmpindexname <- rename(tmpindexname,indexID=SecuCode,
                       indexName=SecuAbbr)

unfroz <- transform(unfroz,proportion=percent(proportion/100,digits = 0),
                    periodrtn_stock=percent(periodrtn_stock),
                    stockName=stockID2name(stockID),
                    periodrtn_index=percent(periodrtn_index))
unfroz <- left_join(unfroz,tmpindexname[,1:2],by='indexID')
unfroz <- unfroz[,c("stockID","stockName","begT","unfroz_date","proportion"              
                    ,"periodrtn_stock","indexName","periodrtn_index")]
colnames(unfroz) <- c('股票代码','股票简称','跟踪起始日','解禁日',"解禁百分比",'区间涨跌幅','所属行业指数','行业指数涨跌幅')


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

########################### ~~ index 000985E ~~ #################################
cat('caculating index 000985E ...\n')
lcdb.update.QT_IndexQuote()
lcdb.update.IndexQuote_000985E()
ewMarket <- getIndexQuote('EI000985E',begT = as.Date('2011-08-02'),variables = c('close','pct_chg'))
indexs <- c('EI000984','EI000982')
tmp <- getIndexQuote(indexs,variables = c('pre_close','close'),begT = as.Date('2011-08-02'),datasrc = 'jy')
tmp$pct_chg <- tmp$close/tmp$pre_close-1
ewMarket <- rbind(ewMarket,tmp[,c("stockID","date","close","pct_chg")])
ewMarketts <- reshape2::dcast(ewMarket,date~stockID,value.var = 'close')
ewMarketts <- na.omit(ewMarketts)
ewMarketts <- xts::xts(ewMarketts[,-1],order.by = ewMarketts$date)
colnames(ewMarketts) <- c('中证500等权','沪深300等权','中证全指等权')

ewMarketrtn <- reshape2::dcast(ewMarket,date~stockID,value.var = 'pct_chg')
ewMarketrtn <- na.omit(ewMarketrtn)
ewMarketrtn <- xts::xts(ewMarketrtn[,-1],order.by = ewMarketrtn$date)


ewMarketSum <- as.data.frame(rtn.periods(ewMarketrtn))
tmp <- as.data.frame(rtn.summary(ewMarketrtn)[c(2,3,5),])
ewMarketSum <- rbind(ewMarketSum,tmp)
ewMarketSum <- transform(ewMarketSum,EI000982=percent(EI000982,digits = 1),
                         EI000984=percent(EI000984,digits = 1),
                         EI000985E=percent(EI000985E,digits = 1))
tmp <- rownames(ewMarketSum)
rownames(ewMarketSum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))
colnames(ewMarketSum) <- c('中证500等权','沪深300等权','中证全指等权')


########################### ~~ industry index MA ~~ #################################
cat('caculating industry index MA ...\n')
indexScore <- getIndustryMA(begT=Sys.Date()-730, endT=Sys.Date()-1)
indexScorets <- plyr::ddply(indexScore,'date',summarise,score=mean(score))
indexScorets <- xts::xts(indexScorets[,-1],order.by = indexScorets[,1])
colnames(indexScorets) <- '大盘均线强弱指数'
tmp <- unique(indexScore$date)
indexScoreData <- indexScore[indexScore$date==max(tmp),]
tmp <- indexScore[indexScore$date==tmp[length(tmp)-4],c('stockID','score')]
indexScoreData <- left_join(indexScoreData,tmp,by='stockID')
rownames(indexScoreData) <- NULL
indexScoreData <- arrange(indexScoreData,desc(score.x))
colnames(indexScoreData) <- c('日期','指数代码','行业','最新得分','上周得分')

########################### ~~ index market timing ~~ #################################
cat('caculating index market timing ...\n')
# market timing
hs300 <- LLT(indexID='EI000300',d=60,trancost=0.001,type='LLT')
cyb <- LLT(indexID='EI399006',d=30,trancost=0.001,type='SMA')
hs300 <- hs300[hs300$date>=as.Date('2011-01-04'),]
cyb <- cyb[cyb$date>=as.Date('2011-01-04'),]

allRtn <- cbind(hs300[,c("date","pct_chg","strRtn")],cyb[,c("pct_chg","strRtn")])
allRtn <- xts::xts(allRtn[,-1],order.by = allRtn[,1])
colnames(allRtn) <- c('hs300','hs300LLT','cyb','cybMA')
allwealth <- WealthIndex(allRtn)
names(allwealth) <- c('沪深300指数','沪深300择时','创业板指数','创业板择时')

allsum <- as.data.frame(rtn.periods(allRtn))
tmp <- as.data.frame(rtn.summary(allRtn)[c(2,3,5),])
allsum <- rbind(allsum,tmp)
allsum <- transform(allsum,hs300=percent(hs300,digits = 1),
                    hs300LLT=percent(hs300LLT,digits = 1),
                    cyb=percent(cyb,digits = 1),
                    cybMA=percent(cybMA,digits = 1))
tmp <- rownames(allsum)
rownames(allsum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))
colnames(allsum) <- c('沪深300指','沪深300LLT','创业板指','创业板MA')

signal <- cbind(hs300[,c("date","close","LLT","signal")],cyb[,c("close",'MA',"signal")])
signal <- signal[(nrow(signal)-9):nrow(signal),]
rownames(signal) <- NULL
colnames(signal) <- c('日期','沪深300收盘价','沪深300LLT','沪深300择时信号','创业板收盘价','创业板MA30','创业板择时信号')



########################### ~~  grid trading ~~ #################################
cat('caculating grid trading ...\n')
indexID <- 'EI000300'
begT <- as.Date('2015-09-01')
endT <- Sys.Date()
para <- list(total=5000000,
          initPos=2,
          posChg=1,
          bar=0.1,
          tradeCost=1/1000)
gridIF300 <- gridTrade.IF(indexID,begT,endT,para)
indexID <- 'EI000905'
gridIF500 <- gridTrade.IF(indexID,begT,endT,para)


indexID <- 'EI000300'
begT <- as.Date('2015-09-01')
endT <- max(gridIF300$date)
para=list(total=5e6,initmv=2e6,bar=0.1,mvChg=1e6,tradeCost=1/1000)
gridIndex300 <- gridTrade.index(indexID,begT,endT,para)
indexID <- 'EI000905'
gridIndex500 <- gridTrade.index(indexID,begT,endT,para) 
gridResult <- cbind(gridIF300[,c('date','benchClose','totalasset')],
                    gridIndex300[,c('totalasset')],
                    gridIF500[,c('benchClose','totalasset')],
                    gridIndex500[,c('totalasset')])
colnames(gridResult) <- c('date','EI000300','grid300IF','grid300Index',
                          'EI000905','grid500IF','grid500Index') 
gridResult <- xts::xts(gridResult[,-1],order.by = gridResult[,1])
gridRtn <- Returns(gridResult)

gridsum <- as.data.frame(rtn.periods(gridRtn))
tmp <- as.data.frame(rtn.summary(gridRtn)[c(2,3,5),])
gridsum <- rbind(gridsum,tmp)
tmp <- rownames(gridsum)
rownames(gridsum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))
gridsum <- transform(gridsum,EI000300=percent(EI000300,digits = 1),
                     grid300IF=percent(grid300IF,digits = 1),
                     grid300Index=percent(grid300Index,digits = 1),
                     EI000905=percent(EI000905,digits = 1),
                     grid500IF=percent(grid500IF,digits = 1),
                     grid500Index=percent(grid500Index,digits = 1))
colnames(gridsum) <- c('沪深300指数','沪深300网格IF','沪深300网格ETF',
                    '中证500指数','中证500网格IC','中证500网格ETF')

gridWealth <- WealthIndex(gridRtn[,c('grid300IF','grid300Index','grid500IF','grid500Index')])
names(gridWealth) <- c('沪深300网格IF','沪深300网格ETF','中证500网格IC','中证500网格ETF')

########################### ~~  bank rotation ~~ #################################
cat('caculating bank rotation ...\n')
begDate <- as.Date('2013-01-04')
endDate <- Sys.Date()-1
chgBar <- 0.2
bankport <- bank.rotation(begDate,endDate,chgBar)
banknew <- bankport[[1]]
banknew <- banknew[banknew$date==max(banknew$date),]
banknew <- transform(banknew,PB_mrq_=round(banknew$PB_mrq_,digits = 2),
                              F_ROE_1=round(F_ROE_1,digits = 2),
                              factorscore=round(factorscore,digits = 1))
rownames(banknew) <- NULL
banknew[is.na(banknew$mark),'mark'] <- ''
colnames(banknew) <- c('日期','代码','简称','PB','预期ROE','翻倍期','持仓')

bankrtn <- bankport[[2]]
bankrtn <- xts::xts(bankrtn[,-1],order.by = bankrtn[,1])
bankrtnsum <- as.data.frame(rtn.periods(bankrtn))
tmp <- as.data.frame(rtn.summary(bankrtn)[c(2,3,5),])
bankrtnsum <- rbind(bankrtnsum,tmp)
tmp <- rownames(bankrtnsum)
rownames(bankrtnsum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))
bankrtnsum <- transform(bankrtnsum,indexRtn=percent(indexRtn,digits = 1),
                        bankRtn=percent(bankRtn,digits = 1))
colnames(bankrtnsum) <- c('申万银行','银行股轮动')


########################### ~~  super small stock ~~ #################################
cat('caculating uper small stock ...\n')
load("smallStock.RData")
begT <- max(smallStockPort$date)
smallStockPort <- smallStockPort[smallStockPort$date<begT,]
smallStockRtn <- smallStockRtn[zoo::index(smallStockRtn)<begT]

RebDates <- getRebDates(begT,Sys.Date()-1)
RebDates <- RebDates[1:(length(RebDates)-1)]
TS <- getTS(RebDates,indexID = 'EI000985')
TS <- rmSuspend(TS) 
TS <- rmPriceLimit(TS)
TSF <- gf_lcfs(TS,'F000002')
TSF <- na.omit(TSF)
TSF$factorscore <- TSF$factorscore*-1
port <- getPort(TSF,topN = 100)
port <- addwgt2port(port)
smallStockPort <- rbind(smallStockPort,port)
tmpRtn <- port.backtest(port[,c("date","stockID","wgt")],fee.buy = 0.003)
tmpRtn <- tmpRtn[zoo::index(tmpRtn)>=begT]
smallStockRtn <- rbind(smallStockRtn,tmpRtn)
save(smallStockPort, smallStockRtn, file = "smallStock.RData")

tmp.index <- getIndexQuote(c('EI000300','EI000905'),begT = min(zoo::index(smallStockRtn)),
                           endT = max(zoo::index(smallStockRtn)),
                           variables = c('pre_close','close'),datasrc = 'jy')

tmp.index$pct_chg <- tmp.index$close/tmp.index$pre_close-1
tmp.index <- reshape2::dcast(tmp.index,date~stockID,value.var = 'pct_chg')
tmp.index <- xts::xts(tmp.index[,-1],order.by = tmp.index[,1])
allsmallstockrtn <- merge(smallStockRtn,tmp.index)
allsmallstockrtnSum <- as.data.frame(rtn.periods(allsmallstockrtn))
tmp <- as.data.frame(rtn.summary(allsmallstockrtn)[c(2,3,5),])
allsmallstockrtnSum <- rbind(allsmallstockrtnSum,tmp)
tmp <- rownames(allsmallstockrtnSum)
rownames(allsmallstockrtnSum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))

allsmallstockrtnSum <- transform(allsmallstockrtnSum,
                                 portfolioReturns=percent(portfolioReturns,digits = 1),
                                 EI000300=percent(EI000300,digits = 1),
                                 EI000905=percent(EI000905,digits = 1))
colnames(allsmallstockrtnSum) <- c('超级小盘股','沪深300','中证500')

