########################### ~~  super small stock ~~ #################################

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
port <- smallStockPort[smallStockPort$date %in% tail(unique(smallStockPort$date),2),]
tmpRtn <- port.backtest(port[,c("date","stockID","wgt")],fee.buy = 0.003)
tmpRtn <- tmpRtn[zoo::index(tmpRtn)>=begT]
smallStockRtn <- rbind(smallStockRtn,tmpRtn)


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



########################### ~~  bank rotation ~~ #################################
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


