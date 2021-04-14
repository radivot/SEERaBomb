graphics.off();rm(list=ls())#clear plots and environment
library(SEERaBomb);library(tidyverse)
library(survival);library(survminer)
# load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
# d=canc%>%filter(primsite%in%c("C693","C694","C692"),histo3%in%8720:8790)
# d$cancer="uveal"
# d=d%>%filter(!is.na(surv))%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
# d5=d%>%select(yrdx,agedx,sex,surv,status)
# save(d,d5,file="uveal/data/d.RData")
load("uveal/data/d.RData")
load("~/data/mrt/mrtUSA.RData")#loads US mortality data
gp=geom_point(); gl=geom_line()
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
tc=function(sz) theme_classic(base_size=sz);
svts=scale_x_continuous(breaks=seq(0,35,5),limits=c(0,38))
gx=xlab("Years Since Diagnosis")
gy=ylab("Excess Absolute Risk of Mortality")


# geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
# gh=geom_hline(yintercept=1)
# agts=scale_x_continuous(breaks=c(25,50,75))#age times

