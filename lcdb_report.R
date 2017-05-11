#!/usr/bin/env Rscript
library(quantbox)
today <- Sys.Date()
if(trday.is(today)){
  lcdb.update()
  library(rmarkdown)
  library(lubridate)
  current_dir <- "D:/Users/aming.tao/Documents/R/Report"
  if(wday(today)==2){
    filename <- paste('quant_report_factor',rdate2int(today),'.html',sep = '')
    render("D:/Users/aming.tao/Documents/R/Report/QuantReport_MF.Rmd", html_document(),output_file = filename,output_dir = current_dir,encoding = 'UTF-8')
  }else{
    filename <- paste('quant_report',rdate2int(today),'.html',sep = '')
    render("D:/Users/aming.tao/Documents/R/Report/QuantReport.Rmd", html_document(),output_file = filename,output_dir = current_dir,encoding = 'UTF-8')
  }
}

