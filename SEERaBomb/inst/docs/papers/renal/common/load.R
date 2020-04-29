graphics.off();rm(list=ls())#clear plots and environment
library(SEERaBomb)
library(tidyverse)
library(ggsci)
library(survival);library(survminer)
# load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
# d=canc%>%filter(primsite%in%c("C649"),histo3==8310)
# table(d$histo3)
# d$cancer="ccRCC"
# d=d%>%filter(!is.na(surv))%>%print(n=4)
# d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
# d5=d%>%select(yrdx,agedx,sex,surv,status)
# save(d,d5,file="renal/data/d.RData")
load("renal/data/d.RData")
load("~/data/mrt/mrtUSA.RData")#loads US mortality data
