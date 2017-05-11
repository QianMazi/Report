########################### ~~ index futures spread ~~ #################################
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
                        indexClose=round(close-spread,1),
                        spread=round(spread,1),
                        spreadPct=percent(spreadPct,1),
                        spreadPctAna=percent(spreadPctAna,1))
rownames(spreadinfo) <- NULL
spreadinfo <- spreadinfo[,c("date","stockID","close","indexClose","spread","spreadPct","spreadPctAna")]
colnames(spreadinfo) <- c('日期','合约代码','合约收盘价','指数收盘价','基差','基差百分比','年化基差百分比')


################################~~~~option~~~~~#############################################



###############################~~~~market emotion~~~~######################################
begT <-  trday.nearby(max(daily_emo_dat$date),by = 1)
endT <-  trday.nearest(Sys.Date()-1)
if(begT<endT){
  tmp.daily_emo_dat <- MaziBox::rpt.dailyemotion(begT,endT)
  tmp.daily_emo_dat <- rbind(daily_emo_dat,tmp.daily_emo_dat)
}

daily_emo_dat_forplot <- daily_emo_dat[,c("date","zt","dt","sixtyhigh","sixtylow","dtpl","ktpl")]
colnames(daily_emo_dat_forplot) <- c("date","自然涨停","自然跌停","60日新高","60日新低","多头排列","空头排列")
daily_emo_tmp1 <- xts::xts(daily_emo_dat_forplot[,c(2,3)],order.by = daily_emo_dat_forplot[,1])
daily_emo_tmp2 <- xts::xts(daily_emo_dat_forplot[,c(4,5)],order.by = daily_emo_dat_forplot[,1])
daily_emo_tmp3 <- xts::xts(daily_emo_dat_forplot[,c(6,7)],order.by = daily_emo_dat_forplot[,1])




