graphics.off();rm(list=ls())#clear plots and environment
load("pmf/data/d7.RData")
d7%>%group_by(sex)%>%summarize(n=n())
library(cheese)
d7%>%univariate_table()
d7%>%group_by(reg)%>%summarize(cases=n(),O=sum(status))
d7%>%group_by(reg,stcnty)%>%summarize(cases=n(),O=sum(status))%>%filter(cases>20)%>%print(n=100)
# names(canc)
# system.time(load("~/data/SEER/mrgd/cancALL.RData")) # which takes ~65 secs
# (ids=unique(canc$stcnty))  # 626 counties looking at all cancers
load("pmf/data/allfields2019.RData")
(dids=unique(d$stcnty))  # 436 counties with at least one PMF

