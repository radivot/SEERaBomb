graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
head(canc,2)
d=canc%>%filter(primsite=="C421",histo3==9961)%>%print(n=2)
d=d%>%filter(yrdx>2000)%>%print(n=2)
d=d%>%filter(!is.na(surv))%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d$age=cut(d$agedx,c(0,40,60,80,126),include.lowest = T)
d$year=cut(d$yrdx,c(2001,2008,2016),include.lowest = T,dig.lab=4)
d$cancer="PMF"
mkDemographics(d,outDir="pmf/outs")
d5=d%>%select(yrdx,agedx,sex,surv,status)
save(d,d5,file="pmf/data/d.RData")

