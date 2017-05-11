factorIDs <- c("F000003","F000004","F000006",
               "F000008","F000009","F000011","F000013","F000014",
               "F000015","F000016","F000017","F000018")
tmp <- buildFactorLists_lcfs(factorIDs,factorOutlier='boxplot',factorNA='median',factorStd='norm')
FactorLists <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorPar=list(),
                  factorDir=-1),
  buildFactorList(factorFun="gf.NP_YOY",
                  factorPar=list(),
                  factorDir=1),
  factorOutlier='boxplot',factorNA='median',factorStd='norm')
FactorLists <- c(tmp,FactorLists)

begT <- Sys.Date()-lubridate::years(7)
endT <- Sys.Date()-1
RebDates <- getRebDates(begT,endT)
TS <- getTS(RebDates,'EI000985')
TSR <- getTSR(TS)
TSF <- getMultiFactor(TS,FactorLists)
TSFR <- left_join(TSF,TSR,by=c('date','stockID'))
TSFR <- na.omit(TSFR)

forder <-  c('ln_mkt_cap_','pct_chg_per_60_','volatility_','NP_YOY','PB_mrq_')
re <- reg.factor.select(TSFR,forder = forder)
TSFR_res <- re$TSFR
reg_results <- reg.TSFR(TSFR_res)


fNames <- guess_factorNames(TSFR_res)
fRtn <- getfRtn(RebDates[86],fNames,reg_results=reg_results,type = 'forcast')
fCov <- calfCov(RebDates,fNames,reg_results=reg_results)
TSF_res <- TSF[TSF$date==max(TSF$date),]
alphaf <- 
port <- OptWgt(TSF_res,fRtn = fRtn,fCov = fCov)


