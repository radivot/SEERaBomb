graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci)#load packages          
library(survival);library(survminer)
load("~/data/SEER/mrgd/cancDef.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
load("~/data/abomb/abomb.RData")#load A-bomb data
load("~/data/usMort/mrt.RData")#loads US mortality data
gp=geom_point();gl=geom_line()#define acronyms
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
RR1=geom_abline(intercept=1,slope=0)
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
svts=scale_x_continuous(breaks=c(0,5,10))#survival times
leg=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
sb=theme(strip.background=element_blank())
ltp=theme(legend.position="top")
sy=scale_y_log10()
tc=theme_classic();jco=scale_color_jco()
# tc12=theme_classic(base_size=12);
# tc14=theme_classic(base_size=14);