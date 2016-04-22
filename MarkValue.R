suppressMessages(library(RFactorModel))
suppressMessages(library(plyr))
getValue <- function(index='EI000985',valtype=c('PE','PB'),caltype=c('median','mean','wgtmean'),begT=as.Date('2006-01-04'),endT=Sys.Date()){
  valtype <- match.arg(valtype)
  caltype <- match.arg(caltype)
  RebDates <- getRebDates(begT,endT,'day')
  TS <- getTS(RebDates,indexID = 'EI000300')
  if(valtype=='PE'){
    TSF <- gf.PE_ttm(TS)
    if(caltype=='median'){
      re <- ddply(TSF,.(date),summarise,PE=median(factorscore))
    }else if(caltype=='mean'){
      TSF <- TSF[TSF$factorscore>0,]
      
    }else{
      
    }
    
  }else{
    gf.PB_mrq()
  }
  
  
  
}

RFactorModel:::factor.outlier
function (TSF, factorOutlier) {
  TSF <- ddply(TSF,"date",
               function(x,outlier){  
                 outlier_u <- with(x,mean(factorscore,na.rm=TRUE)+outlier*sd(factorscore,na.rm=TRUE))
                 outlier_l <- with(x,mean(factorscore,na.rm=TRUE)-outlier*sd(factorscore,na.rm=TRUE))
                 transform(x,factorscore = ifelse(factorscore > outlier_u, outlier_u,
                                                  ifelse(factorscore < outlier_l, outlier_l, factorscore)))
               },outlier=factorOutlier)
  return(TSF)
}


