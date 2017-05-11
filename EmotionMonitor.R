
library(WindR)
w.start()
library(RFactorModel)
library()

######## Money flows
begT <-  trday.nearest(Sys.Date()-365)
endT <-  trday.nearest(Sys.Date()-1)
daily_emo_dat <- MaziBox::rpt.dailyemotion(begT,endT)


