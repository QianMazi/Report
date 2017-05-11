########################### ~~  super small stock ~~ #################################

bigport <- sizePortData$big
smallport <- sizePortData$small
sizeRtn <- sizePortData$rtn
begT <- max(bigport$date)
bigport <- bigport[bigport$date<begT,]
smallport <- smallport[smallport$date<begT,]
sizeRtn <- sizeRtn[zoo::index(sizeRtn)<=begT]

RebDates <- getRebDates(begT,Sys.Date()-1)
RebDates <- RebDates[1:(length(RebDates)-1)]
TS <- getTS(RebDates,indexID = 'EI000985')
TS <- rmSuspend(TS) 
TSF <- gf_lcfs(TS,'F000002')
TSF <- na.omit(TSF)

tmp.bigport <- TSF %>% arrange(date,desc(factorscore)) %>% group_by(date) %>% 
  slice(1:100) %>% ungroup() %>% mutate(wgt=0.01)
tmp.smallport <- TSF %>% arrange(date,factorscore) %>% group_by(date) %>% 
  slice(1:100) %>% ungroup() %>% mutate(wgt=0.01)
tmp.bigport <- as.data.frame(tmp.bigport)
tmp.smallport <- as.data.frame(tmp.smallport)

bigport <- rbind(bigport,tmp.bigport)
smallport <- rbind(smallport,tmp.smallport)

tmpRtn1 <- port.backtest(tmp.bigport,fee.buy = 0.003)
tmpRtn2 <- port.backtest(tmp.smallport,fee.buy = 0.003)
tmp.sizeRtn <- merge(tmpRtn1,tmpRtn2)
colnames(tmp.sizeRtn) <- c('big','small')
tmp.sizeRtn <- tmp.sizeRtn[zoo::index(tmp.sizeRtn)>begT]
sizeRtn <- rbind(sizeRtn,tmp.sizeRtn)
sizePortData <- list(big=bigport,small=smallport,rtn=sizeRtn)

tmp.index <- getIndexQuote('EI801003',begT = min(zoo::index(sizeRtn)),
                           endT = max(zoo::index(sizeRtn)),
                           variables = c('pre_close','close'),datasrc = 'jy')
tmp.index$pct_chg <- tmp.index$close/tmp.index$pre_close-1
tmp.index <- xts::xts(tmp.index[,'pct_chg'],order.by = tmp.index[,'date'])
colnames(tmp.index) <- 'index'
allsmallstockrtn <- merge(sizeRtn,tmp.index)
colnames(allsmallstockrtn) <- c('超大盘100','超小盘100','申万A股')
allsmallstockrtnSum <- as.data.frame(rtn.periods(allsmallstockrtn))
tmp <- as.data.frame(rtn.summary(allsmallstockrtn)[c(2,3,5),])
allsmallstockrtnSum <- rbind(allsmallstockrtnSum,tmp)
tmp <- rownames(allsmallstockrtnSum)
rownames(allsmallstockrtnSum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))

allsmallstockrtnSum[,1] <- percent(allsmallstockrtnSum[,1],digits = 1)
allsmallstockrtnSum[,2] <- percent(allsmallstockrtnSum[,2],digits = 1)
allsmallstockrtnSum[,3] <- percent(allsmallstockrtnSum[,3],digits = 1)




########################### ~~  bank rotation ~~ #################################
begDate <- as.Date('2017-01-04')
endDate <- Sys.Date()-1
chgBar <- 0.1
bankport$TSF <- bankport$TSF[bankport$TSF$date<begDate,]
bankport$rtn <- bankport$rtn[zoo::index(bankport$rtn)<begDate]

tmp.bankport <- bank.rotation(begDate,endDate,chgBar)
bankport$TSF <- rbind(bankport$TSF,tmp.bankport$TSF)
bankport$rtn <- rbind(bankport$rtn,tmp.bankport$rtn)

banknew <- bankport[[1]]
banknew <- banknew[banknew$date==max(banknew$date),]
banknew <- transform(banknew,PB=round(PB,digits = 2),
                              ROE=round(ROE,digits = 2),
                              factorscore=round(factorscore,digits = 1))
rownames(banknew) <- NULL
banknew[is.na(banknew$mark),'mark'] <- ''
colnames(banknew) <- c('日期','代码','简称','PB','ROE','翻倍期','持仓')

bankrtn <- bankport[[2]]
bankrtnsum <- as.data.frame(rtn.periods(bankrtn))
tmp <- as.data.frame(rtn.summary(bankrtn)[c(2,3,5),])
bankrtnsum <- rbind(bankrtnsum,tmp)
tmp <- rownames(bankrtnsum)
rownames(bankrtnsum) <- c(tmp[1:(length(tmp)-5)],c('累计收益率','年化收益率','年化波动率','年化Sharpe值','最大回撤'))
bankrtnsum <- transform(bankrtnsum,indexRtn=percent(indexRtn,digits = 1),
                        bankRtn=percent(bankRtn,digits = 1))
colnames(bankrtnsum) <- c('申万银行','银行股轮动')


