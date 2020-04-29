graphics.off();rm(list=ls())#clear plots and environment
# library(tidyverse);library(SEERaBomb)
library(SEERaBomb)
load("renal/data/d.RData") #see load.R in common to make this
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
d$age=cut(d$agedx,c(0,40,60,80,126),include.lowest = T)
d$year=cut(d$yrdx,c(1975,1995,2016),include.lowest = T,dig.lab=4)
mkDemographics(d,outDir="renal/outs")
