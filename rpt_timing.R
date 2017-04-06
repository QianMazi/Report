########################### ~~ index 000985E ~~ #################################

ewMarket <- getIndexQuote('EI000985E',begT = as.Date('2011-08-02'),variables = c('close','pct_chg'))
tmp <- getIndexQuote(c('EI000984','EI000982'),variables = c('pre_close','close'),begT = as.Date('2011-08-02'),datasrc = 'jy')
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
# market timing
hs300 <- LLT(indexID='EI000300',begT = as.Date("2010-01-04"),d=60,trancost=0.001,type='LLT')
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

