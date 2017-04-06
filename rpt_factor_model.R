##############################~~load weekly data~~######################################
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


RebDates <- trday.offset(trday.nearest(Sys.Date()-1),by=lubridate::weeks(-1))
for(i in 1:3){
  tmp <- trday.offset(min(RebDates),by=lubridate::weeks(-1))
  RebDates <- c(RebDates,tmp)
}
RebDates <- sort(RebDates)

mp <- modelPar.default()
mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
TS <- getTS(RebDates,'EI000985')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates,'EI000300')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_hs300 <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates,'EI000905')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_zz500 <- Model.TSFs_byTS(MPs=mps,TS=TSR)


######################################~~weekly IC~~##################################
ic_week_all <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000985',
                          plyr::ldply(TSFRs,seri.IC,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000300',
                            plyr::ldply(TSFRs_hs300,seri.IC,.id = 'fname'))
ic_week_all <- rbind(ic_week_all,tmp)
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000905',
                   plyr::ldply(TSFRs_zz500,seri.IC,.id = 'fname'))
ic_week_all <- rbind(ic_week_all,tmp)


IC_week_summary_table <- data.frame(cbind(MC.table.IC(TSFRs)[,c(1,6)],
                           MC.table.IC(TSFRs_hs300)[,c(1,6)],
                           MC.table.IC(TSFRs_zz500)[,c(1,6)]))
IC_week_summary_table <- transform(IC_week_summary_table,
                                   fname=rownames(IC_week_summary_table),
                                   order1=rank(-IC_mean),
                                   order2=rank(-IC_mean.1),
                                   order3=rank(-IC_mean.2),
                                   IC_mean=round(IC_mean,3),
                                   IC_mean.1=round(IC_mean.1,3),
                                   IC_mean.2=round(IC_mean.2,3))
rownames(IC_week_summary_table) <- NULL
IC_week_summary_table <- dplyr::arrange(IC_week_summary_table,order1)
IC_week_summary_table <- IC_week_summary_table[,c("fname","order1","IC_mean","IC_hitRatio","order2",
                                                  "IC_mean.1","IC_hitRatio.1","order3","IC_mean.2","IC_hitRatio.2"                             )]
colnames(IC_week_summary_table) <- c('因子名','全市场排名','全市场均值','全市场胜率',
                                     '沪深300排名','沪深300均值','沪深300胜率',
                                     '中证500排名','中证500均值','中证500胜率')

######################################~~weekly Ngroup return~~##################################
ngroup_week_all <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000985',
                              plyr::ldply(TSFRs,seri.Ngroup.rtn,N=10,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000300',
                  plyr::ldply(TSFRs_hs300,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_week_all <- rbind(ngroup_week_all,tmp)
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000905',
                  plyr::ldply(TSFRs_zz500,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_week_all <- rbind(ngroup_week_all,tmp)
ngroup_week_all <- dplyr::mutate(ngroup_week_all,Q1=Q1*100,Q10=Q10*100,Q1_Q10=Q1-Q10)
ngroup_week_all <- ngroup_week_all[,c("date","indexID","fname","Q1","Q10","Q1_Q10")]

ngroup_week_all <- reshape2::melt(ngroup_week_all,id=c("date","indexID","fname"),variable.name = "group",
                                  value.name = "return")


ngroup_week_all_sum <- ngroup_week_all %>% filter(group=='Q1') %>% group_by(indexID,fname) %>% 
  summarise(return=(prod(1+return/100)-1)*100) %>% ungroup()
ngroup_week_all_sum <- reshape2::dcast(ngroup_week_all_sum,
                                       fname~indexID,value.var ='return')
colnames(ngroup_week_all_sum) <- c("fname","EI000985Q1","EI000300Q1","EI000905Q1")
tmp <- ngroup_week_all %>% filter(group=='Q10') %>% group_by(indexID,fname) %>% 
  summarise(return=(prod(1+return/100)-1)*100) %>% ungroup()
tmp <- reshape2::dcast(tmp,fname~indexID,value.var ='return')
colnames(tmp) <- c("fname","EI000985Q10","EI000300Q10","EI000905Q10")
ngroup_week_all_sum <- cbind(ngroup_week_all_sum,tmp[,-1])


ngroup_week_all_sum <- transform(ngroup_week_all_sum,
                                    EI000985Q1=round(EI000985Q1,1),
                                    EI000985Q10=round(EI000985Q10,1),
                                    EI000300Q1=round(EI000300Q1,1),
                                    EI000300Q10=round(EI000300Q10,1),
                                    EI000905Q1=round(EI000905Q1,1),
                                    EI000905Q10=round(EI000905Q10,1))
ngroup_week_all_sum <- dplyr::arrange(ngroup_week_all_sum,desc(EI000985Q1))
ngroup_week_all_sum <- ngroup_week_all_sum[,c("fname","EI000985Q1","EI000985Q10","EI000300Q1","EI000300Q10","EI000905Q1","EI000905Q10")]
colnames(ngroup_week_all_sum) <- c('因子名','全市场Q1','全市场Q10',
                                     '沪深300Q1','沪深300Q10',
                                     '中证500Q1','中证500Q10')

#################################~~~load monthly data~~##########################
RebDates <- trday.offset(trday.nearest(Sys.Date()-1),by=months(-1))
for(i in 1:11){
  tmp <- trday.offset(min(RebDates),by=months(-1))
  RebDates <- c(RebDates,tmp)
}
RebDates <- sort(RebDates)

mp <- modelPar.default()
mps <- getMPs_FactorLists(FactorLists,modelPar=mp)
TS <- getTS(RebDates,'EI000985')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates,'EI000300')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_hs300 <- Model.TSFs_byTS(MPs=mps,TS=TSR)

TS <- getTS(RebDates,'EI000905')
TS <- rm_suspend(TS)
TSR <- getTSR(TS)
TSFRs_zz500 <- Model.TSFs_byTS(MPs=mps,TS=TSR)

######################################~~monthly IC~~##################################
ic_month_all <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000985',
                          plyr::ldply(TSFRs,seri.IC,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000300',
                  plyr::ldply(TSFRs_hs300,seri.IC,.id = 'fname'))
ic_month_all <- rbind(ic_month_all,tmp)
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000905',
                  plyr::ldply(TSFRs_zz500,seri.IC,.id = 'fname'))
ic_month_all <- rbind(ic_month_all,tmp)


IC_month_summary_table <- data.frame(cbind(MC.table.IC(TSFRs)[,c(1,6)],
                                          MC.table.IC(TSFRs_hs300)[,c(1,6)],
                                          MC.table.IC(TSFRs_zz500)[,c(1,6)]))
IC_month_summary_table <- transform(IC_month_summary_table,
                                   fname=rownames(IC_month_summary_table),
                                   order1=rank(-IC_mean),
                                   order2=rank(-IC_mean.1),
                                   order3=rank(-IC_mean.2),
                                   IC_mean=round(IC_mean,3),
                                   IC_mean.1=round(IC_mean.1,3),
                                   IC_mean.2=round(IC_mean.2,3))
rownames(IC_month_summary_table) <- NULL
IC_month_summary_table <- dplyr::arrange(IC_month_summary_table,order1)
IC_month_summary_table <- IC_month_summary_table[,c("fname","order1","IC_mean","IC_hitRatio","order2",
                                                  "IC_mean.1","IC_hitRatio.1","order3","IC_mean.2","IC_hitRatio.2"                             )]
colnames(IC_month_summary_table) <- c('因子名','全市场排名','全市场均值','全市场胜率',
                                     '沪深300排名','沪深300均值','沪深300胜率',
                                     '中证500排名','中证500均值','中证500胜率')



######################################~~monthly Ngroup return~~##################################
ngroup_month_all <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000985',
                              plyr::ldply(TSFRs,seri.Ngroup.rtn,N=10,.id = 'fname'))
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000300',
                  plyr::ldply(TSFRs_hs300,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_month_all <- rbind(ngroup_month_all,tmp)
tmp <- data.frame(date=rep(RebDates,length(FactorLists)),indexID='EI000905',
                  plyr::ldply(TSFRs_zz500,seri.Ngroup.rtn,N=10,.id = 'fname'))
ngroup_month_all <- rbind(ngroup_month_all,tmp)
ngroup_month_all <- dplyr::mutate(ngroup_month_all,Q1=Q1*100,Q10=Q10*100,Q1_Q10=Q1-Q10)
ngroup_month_all <- ngroup_month_all[,c("date","indexID","fname","Q1","Q10","Q1_Q10")]

ngroup_month_all <- reshape2::melt(ngroup_month_all,id=c("date","indexID","fname"),variable.name = "group",
                                  value.name = "return")


ngroup_month_all_sum <- ngroup_month_all %>% filter(group=='Q1') %>% group_by(indexID,fname) %>% 
  summarise(return=(prod(1+return/100)-1)*100) %>% ungroup()
ngroup_month_all_sum <- reshape2::dcast(ngroup_month_all_sum,
                                       fname~indexID,value.var ='return')
colnames(ngroup_month_all_sum) <- c("fname","EI000985Q1","EI000300Q1","EI000905Q1")
tmp <- ngroup_month_all %>% filter(group=='Q10') %>% group_by(indexID,fname) %>% 
  summarise(return=(prod(1+return/100)-1)*100) %>% ungroup()
tmp <- reshape2::dcast(tmp,fname~indexID,value.var ='return')
colnames(tmp) <- c("fname","EI000985Q10","EI000300Q10","EI000905Q10")
ngroup_month_all_sum <- cbind(ngroup_month_all_sum,tmp[,-1])


ngroup_month_all_sum <- transform(ngroup_month_all_sum,
                                 EI000985Q1=round(EI000985Q1,1),
                                 EI000985Q10=round(EI000985Q10,1),
                                 EI000300Q1=round(EI000300Q1,1),
                                 EI000300Q10=round(EI000300Q10,1),
                                 EI000905Q1=round(EI000905Q1,1),
                                 EI000905Q10=round(EI000905Q10,1))
ngroup_month_all_sum <- dplyr::arrange(ngroup_month_all_sum,desc(EI000985Q1))
ngroup_month_all_sum <- ngroup_month_all_sum[,c("fname","EI000985Q1","EI000985Q10","EI000300Q1","EI000300Q10","EI000905Q1","EI000905Q10")]
colnames(ngroup_month_all_sum) <- c('因子名','全市场Q1','全市场Q10',
                                   '沪深300Q1','沪深300Q10',
                                   '中证500Q1','中证500Q10')




