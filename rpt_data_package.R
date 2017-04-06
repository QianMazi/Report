library(ggplot2)
library(quantbox)
library(WindR)
w.start(showmenu = F)
tsInclude()
tsConnect()
load("rptData.RData")

lcdb.update.QT_IndexTiming()
lcdb.update.QT_DailyQuote2()
lcdb.update.QT_IndexQuote()
lcdb.update.IndexQuote_000985E()




