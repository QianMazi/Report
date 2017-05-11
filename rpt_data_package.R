library(ggplot2)
library(quantbox)
library(WindR)
w.start(showmenu = F)
tsInclude()
tsConnect()
load("rptData.RData")
lcdb.update.QT_IndexTiming()
MaziBox::lcdb.build.EE_ForecastAndReport()



