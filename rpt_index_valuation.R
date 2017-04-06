########################### ~~ index valuation ~~ #################################

#get index valuation result
indexValue <- getIV(valtype = 'PE')
tmp <- getIV(valtype = 'PB')
tmp <- dplyr::select(tmp,-indexName)
indexValue <- dplyr::left_join(indexValue,tmp,by=c("date","indexID"))
colnames(indexValue) <- c("indexID","indexName","date","PE_median","perPE","PB_median","perPB")

tablevaluation <- indexValue[indexValue$date==max(indexValue$date),]
tablevaluation <- arrange(tablevaluation,desc(perPE+perPB))
tablevaluation <- transform(tablevaluation,
                            PE_median=round(PE_median,1),
                            perPE=percent(perPE,digits = 0),
                            PB_median=round(PB_median,1),
                            perPB=percent(perPB,digits = 0))
colnames(tablevaluation) <- c('指数代码','指数简称','日期','动态市盈率','市盈率百分位','动态市净率','市净率百分位')

valuets <- dplyr::filter(indexValue,indexID %in% c('EI801003','EI000300','EI399006'),
                         date>=(max(date)-months(18)))

################################ special valuation ###############################
endT <- Sys.Date()-1

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
  tmp.commodity_price$goldsilver <- tmp.commodity_price[,'GC00.CMX']/tmp.commodity_price[,'SI00.CMX']
  tmp.commodity_price$goldoil <- tmp.commodity_price[,'GC00.CMX']/tmp.commodity_price[,'CL00.NYM']
  commodity_price <- rbind(commodity_price,tmp.commodity_price)
  commodity_price <- zoo::na.locf(commodity_price)
  
}


value.bond_yield <- TTR::runPercentRank(bond_yield, n = 250, cumulative = T, exact.multiplier = 0.5)
colnames(value.bond_yield) <- '10年期国债收益率'
value.bond_yield <- value.bond_yield['2015/']


value.tmp1 <- TTR::runPercentRank(commodity_price[,'goldsilver'], n = 250, cumulative = T, exact.multiplier = 0.5)
value.tmp2 <- TTR::runPercentRank(commodity_price[,'goldoil'], n = 250, cumulative = T, exact.multiplier = 0.5)
value.commodity_price <-  merge(value.tmp1,value.tmp2)
colnames(value.commodity_price) <- c('金银比','金油比')
value.commodity_price <- value.commodity_price['2015/']

value.all <- merge(value.bond_yield,value.commodity_price)
value.all <- zoo::na.locf(value.all)
value.all <- na.omit(value.all)
