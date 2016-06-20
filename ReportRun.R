suppressMessages(library(RFactorModel))
suppressMessages(library(plyr))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(reshape2))
suppressMessages(library(TTR))

source('ReportFun.R', encoding = 'UTF-8', echo=F)

lcdb.update.QT_IndexTiming(type='update')

#get index valuation result
indexValue <- getIV()
tmp <- getIV(valtype = 'PB',caltype = 'median')
tablevaluation <- indexValue$new
tmp <- tmp$new
tmp <- tmp[,c("日期","指数代码","指数简称","动态市净率","市净率百分位")]
tablevaluation <- merge(tablevaluation,tmp,by=c("日期","指数代码","指数简称"))
tablevaluation <- arrange(tablevaluation,desc(tablevaluation[,6]))


#get index futures spread info
IFSpread <- getIFSpread()


#get resume stock arbitrage opportunity
endDate <- Sys.Date()
begDate <- trday.nearby(endDate, by = 1)
resumeStock <- resumeArbitrage(begDate,endDate)
if(!is.character(resumeStock)){
  colnames(resumeStock) <-c('基金代码','基金简称','基金类型','复牌股票简称','停牌时占指数比重',
                            '停牌日期','复牌日期','所属行业','停牌期间行业涨跌幅',
                            '停牌前场内份额','复牌后场内份额','预估最新权重') 
}


indexScore <- getMASeries()


# market timing
hs300 <- LLT(indexID='EI000300',d=60,trancost=0.001,type='LLT')
cyb <- LLT(indexID='EI399006',d=30,trancost=0.001,type='SMA')

allwealth <- WealthIndex(hs300$rtn)
tmp <- WealthIndex(cyb$rtn)
allwealth <- merge(allwealth,tmp)
names(allwealth) <- c('沪深300指数净值','沪深300择时净值','创业板指净值','创业板择时净值')


allsum <- as.data.frame(round(rtn.summary(hs300$rtn),digits = 3)*100)
tmp <- as.data.frame(round(rtn.summary(cyb$rtn),digits = 3)*100)
allsum <- cbind(allsum,tmp)
allsum <- allsum[c("Annualized Return","Annualized Std Dev","Annualized Sharpe (Rf=0%)"
                   ,"Worst Drawdown" ),]

rownames(allsum) <- c('年化收益率','年化波动率','年化Sharpe值','最大回撤')
colnames(allsum) <- c('沪深300指','沪深300LLT','创业板指','创业板MA')

signal <- hs300$rawdata
signal <- signal[(nrow(signal)-9):nrow(signal),c('date','close','signal')]
tmp <- cyb$rawdata
tmp <- tmp[(nrow(tmp)-9):nrow(tmp),c('date','close','signal')]
signal <- merge(signal,tmp,by='date')
colnames(signal) <- c('日期','沪深300收盘价','沪深300择时信号','创业板收盘价','创业板择时信号')


indexID='EI000300'
begT=as.Date('2015-09-01')
endT=Sys.Date()
para=list(total=5000000,
          initPos=2,
          bar=0.1,
          tradeCost=2/10000,
          multiplier=200)

gridIF300 <- gridTrade.IF(indexID,begT,endT,para)
indexID='EI000905'
gridIF500 <- gridTrade.IF(indexID,begT,endT,para)


indexID <- 'SH000300'
fundID <- 'SH510300'
begT <- as.Date('2015-09-01')
endT <- max(gridIF300$date)
para=list(total=500000,
          initmv=200000,
          bar=0.05,
          mvChg=50000,
          tradeCost=1/1000)
gridIndex300 <- gridTrade.index(indexID,fundID,begT,endT,para)
indexID <- 'SH000905'
fundID <- 'SH510500'
gridIndex500 <- gridTrade.index(indexID,fundID,begT,endT,para) 
gridResult <- cbind(gridIF300[,c('date','benchClose','totalasset')],
                    gridIndex300[,c('totalasset')],
                    gridIF500[,c('benchClose','totalasset')],
                    gridIndex500[,c('totalasset')])
colnames(gridResult) <- c('date','EI000300','grid300IF','grid300Index',
                          'EI000905','grid500IF','grid500Index') 
gridResult <- as.xts(gridResult[,-1],order.by = gridResult[,1])
gridRtn <- Returns(gridResult)
names(gridRtn) <- c('沪深300','沪深300网格IF','沪深300网格ETF',
                       '中证500','中证500网格IC','中证500网格ETF')
gridsum <- as.data.frame(round(rtn.summary(gridRtn),digits = 3)*100)
gridWealth <- WealthIndex(gridRtn)

