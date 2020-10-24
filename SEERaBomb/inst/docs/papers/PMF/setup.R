graphics.off();rm(list=ls())#clear plots and environment
library(SEERaBomb);library(tidyverse);library(WriteXLS)
library(survival);library(survminer)
library(bbmle); library(broom)
load("pmf/data/d.RData") # made in demog.R
load("~/data/mrt/mrtUSA.RData")#loads US mortality data
gp=geom_point(); gl=geom_line()
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
tc=function(sz) theme_classic(base_size=sz);
svts=scale_x_continuous(breaks=seq(0,13,2),limits=c(0,13))
RRvals=scale_y_continuous(breaks=seq(0,10,2),limits=c(0,10))
gx=xlab("Years Since Diagnosis")
gy=ylab("Excess Absolute Risk of Mortality")
gyRR=ylab("Relative Risk of Mortality")
gh0=geom_hline(yintercept=0)
gh1=geom_hline(yintercept=1)

tableD=function(D) {
  D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR),
                         " (",sprintf('%.3f',LL),
                         ", ",sprintf('%.3f',UL),")"))
  D=D%>%mutate(Time=sprintf('%.2f',t))
  D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
    select(Group,
           Interval=int,
           "Time in Years"=Time,
           "Excess Absolute Risk (O-E)/PY"=CI,
           "Deaths Observed"=O,
           "Deaths Expected"=E,
           "Person Years"=PY)
}

