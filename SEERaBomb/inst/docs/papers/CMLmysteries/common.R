### common.R  common block sourced in by other scripts
graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci);library(bbmle)
load("~/data/SEER/mrgd/cancDef.RData")#load SEER cancer data
gp=geom_point();gl=geom_line()
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.1,alpha=0.4)
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1,alpha=0.4)#for absolute risks
gh=geom_hline(yintercept=1)
svts=scale_x_continuous(breaks=c(0,5,10))#surv times
agts=scale_x_continuous(breaks=c(25,50,75))#age times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
sy=scale_y_log10()
cc=coord_cartesian(ylim=c(0,10))#clips high errorbars
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
gxi=xlab("Age (Years)")
gyi=ylab(quote(paste("Cases per ",10^5," Person Years")))

