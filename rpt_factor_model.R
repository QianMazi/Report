##############################~~load data~~######################################
factorIDs <- c("F000001","F000003","F000004","F000006","F000007",
               "F000008","F000009","F000010","F000011","F000012",
               "F000013","F000014","F000015","F000016","F000017","F000018")
tmp <- buildFactorLists_lcfs(factorIDs,factorOutlier='boxplot')
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="gf.G_MLL_Q",
                  factorPar=list(),
                  factorDir=1),
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  buildFactorList(factorFun="gf.G_OCF_Q",
                  factorPar=list(),
                  factorDir=1),
  factorOutlier='boxplot')
FactorLists <- c(tmp,FactorLists)


RebDates_week <- trday.offset(trday.nearest(Sys.Date()-1),by=lubridate::weeks(-1))
for(i in 1:3){
  tmp <- trday.offset(min(RebDates_week),by=lubridate::weeks(-1))
  RebDates_week <- c(RebDates_week,tmp)
}
RebDates_week <- sort(RebDates_week)

mp <- modelPar.default()
mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
TS <- getTS(RebDates_week,'EI000985')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_week <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates_week,'EI000300')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_week_hs300 <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates_week,'EI000905')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_week_zz500 <- Model.TSFs_byTS(MPs=mps,TS=TSR)


RebDates_month <- trday.offset(trday.nearest(Sys.Date()-1),by=months(-1))
for(i in 1:11){
  tmp <- trday.offset(min(RebDates_month),by=months(-1))
  RebDates_month <- c(RebDates_month,tmp)
}
RebDates_month <- sort(RebDates_month)

TS <- getTS(RebDates_month,'EI000985')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_month <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates_month,'EI000300')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_month_hs300 <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates_month,'EI000905')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_month_zz500 <- Model.TSFs_byTS(MPs=mps,TS=TSR)



######################################~~ IC~~##################################
ic_all <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000985',period='week',
                          plyr::ldply(TSFRs_week,seri.IC,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000300',period='week',
                            plyr::ldply(TSFRs_week_hs300,seri.IC,.id = 'fname'))
ic_all <- rbind(ic_all,tmp)
tmp <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000905',period='week',
                   plyr::ldply(TSFRs_week_zz500,seri.IC,.id = 'fname'))
ic_all <- rbind(ic_all,tmp)

tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000985',period='month',
                           plyr::ldply(TSFRs_month,seri.IC,.id = 'fname'))
ic_all <- rbind(ic_all,tmp)
tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000300',period='month',
                  plyr::ldply(TSFRs_month_hs300,seri.IC,.id = 'fname'))
ic_all <- rbind(ic_all,tmp)
tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000905',period='month',
                  plyr::ldply(TSFRs_month_zz500,seri.IC,.id = 'fname'))
ic_all <- rbind(ic_all,tmp)



IC_summary_table <- ic_all %>% group_by(period,indexID,fname) %>% 
  summarise(mean=mean(IC),hitratio=sum(IC>0)/n()) %>% ungroup()
ic_all <- left_join(ic_all,IC_summary_table,by=c('indexID','period','fname'))
ic_all <- transform(ic_all,tag=ifelse(mean>0 & hitratio>=0.5,'alpha','risk'))

IC_summary_table <- IC_summary_table %>% group_by(period,indexID) %>% 
  mutate(order=rank(-mean),mean=round(mean,3),hitratio=round(hitratio,2))

IC_summary_table.IC <- reshape2::dcast(IC_summary_table,period+fname~indexID,
                                       value.var = 'mean')
IC_summary_table.hitratio <- reshape2::dcast(IC_summary_table,period+fname~indexID,
                                       value.var = 'hitratio')
IC_summary_table.order <- reshape2::dcast(IC_summary_table,period+fname~indexID,
                                       value.var = 'order')
IC_summary_table <- left_join(IC_summary_table.IC,IC_summary_table.hitratio,
                              by=c('period','fname'))
IC_summary_table <- left_join(IC_summary_table,IC_summary_table.order,
                              by=c('period','fname'))


IC_summary_table <- dplyr::arrange(IC_summary_table,period,EI000985)
IC_summary_table <- IC_summary_table[,c("period","fname","EI000985","EI000985.x","EI000985.y","EI000300",  
                                        "EI000300.x","EI000300.y","EI000905","EI000905.x", "EI000905.y")]
colnames(IC_summary_table) <- c('period','因子名','全市场排名','全市场均值','全市场胜率',
                                     '沪深300排名','沪深300均值','沪深300胜率',
                                     '中证500排名','中证500均值','中证500胜率')

######################################~~weekly Ngroup return~~##################################
#get index period return
dates <- rbind(unique(TSFRs_week[[1]][,c('date','date_end')]),
               unique(TSFRs_month[[1]][,c('date','date_end')]))
indexret <- rbind(getPeriodrtn_EI(stockID="EI000985",begT=dates$date, endT=dates$date_end),
                  getPeriodrtn_EI(stockID="EI000300",begT=dates$date, endT=dates$date_end),
                  getPeriodrtn_EI(stockID="EI000905",begT=dates$date, endT=dates$date_end))
colnames(indexret) <- c('indexID','date','endT','benchrtn')
indexret$endT <- NULL
indexret$benchrtn <- indexret$benchrtn*100

#get ngroup return
ngroup_all <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000985',period='week',
                              plyr::ldply(TSFRs_week,seri.Ngroup.rtn,N=10,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000300',period='week',
                  plyr::ldply(TSFRs_week_hs300,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_all <- rbind(ngroup_all,tmp)
tmp <- data.frame(date=rep(RebDates_week,length(FactorLists)),indexID='EI000905',period='week',
                  plyr::ldply(TSFRs_week_zz500,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_all <- rbind(ngroup_all,tmp)

tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000985',period='month',
                               plyr::ldply(TSFRs_month,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_all <- rbind(ngroup_all,tmp)
tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000300',period='month',
                  plyr::ldply(TSFRs_month_hs300,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_all <- rbind(ngroup_all,tmp)
tmp <- data.frame(date=rep(RebDates_month,length(FactorLists)),indexID='EI000905',period='month',
                  plyr::ldply(TSFRs_month_zz500,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_all <- rbind(ngroup_all,tmp)

ngroup_all <- dplyr::mutate(ngroup_all,Q1=Q1*100,Q10=Q10*100,Q1_Q10=Q1-Q10)
ngroup_all <- ngroup_all[,c("period","date","indexID","fname","Q1","Q10","Q1_Q10")]

ngroup_all <- reshape2::melt(ngroup_all,id=c("period","date","indexID","fname"),variable.name = "group",
                                  value.name = "return")
ngroup_all <- left_join(ngroup_all,indexret,by=c('date','indexID'))
ngroup_all$return <- ngroup_all$return-ngroup_all$benchrtn


ngroup_all_sum <- ngroup_all %>% filter(group=='Q1') %>% group_by(period,indexID,fname) %>% 
  summarise(totalreturn=(prod(1+return/100)-1)*100,pct=sum(return>0)/n()) %>% ungroup()
ngroup_all <- left_join(ngroup_all,ngroup_all_sum,by=c('period','indexID','fname'))
ngroup_all <- transform(ngroup_all,tag=ifelse(totalreturn>0 &  pct>=0.5,'alpha','risk'))

ngroup_all_sum <- reshape2::dcast(ngroup_all_sum,
                                       period+fname~indexID,value.var ='totalreturn')
colnames(ngroup_all_sum) <- c("period","fname","EI000985Q1","EI000300Q1","EI000905Q1")
tmp <- ngroup_all %>% filter(group=='Q10') %>% group_by(period,indexID,fname) %>% 
  summarise(return=(prod(1+return/100)-1)*100) %>% ungroup()
tmp <- reshape2::dcast(tmp,period+fname~indexID,value.var ='return')
colnames(tmp) <- c("period","fname","EI000985Q10","EI000300Q10","EI000905Q10")
ngroup_all_sum <- left_join(ngroup_all_sum,tmp,by=c("period",'fname'))

ngroup_all_sum <- transform(ngroup_all_sum,
                                    EI000985Q1=round(EI000985Q1,1),
                                    EI000985Q10=round(EI000985Q10,1),
                                    EI000300Q1=round(EI000300Q1,1),
                                    EI000300Q10=round(EI000300Q10,1),
                                    EI000905Q1=round(EI000905Q1,1),
                                    EI000905Q10=round(EI000905Q10,1))
ngroup_all_sum <- dplyr::arrange(ngroup_all_sum,period,desc(EI000985Q1))
ngroup_all_sum <- ngroup_all_sum[,c("period","fname","EI000985Q1","EI000985Q10","EI000300Q1","EI000300Q10","EI000905Q1","EI000905Q10")]
colnames(ngroup_all_sum) <- c('period','因子名','全市场Q1','全市场Q10',
                                     '沪深300Q1','沪深300Q10',
                                     '中证500Q1','中证500Q10')







