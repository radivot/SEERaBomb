graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
head(canc,2)
canc=canc%>%filter(primsite%in%c("C693","C694","C692"),histo3%in%8720:8790)%>%print(n=2)
canc$cancer=as.character(canc$cancer)
# C69.3 (choroid), C69.4 (ciliary body and iris), and C69.2 (retina). 
canc$cancer[canc$primsite=="C693"]="Choroid" 
canc$cancer[canc$primsite=="C694"]="Ciliary"
canc$cancer[canc$primsite=="C692"]="Retinal"
canc%>%filter(is.na(surv))%>%print(n=4) # one choroid survival is missing so tables will be shy 1 for it
d=canc%>%filter(!is.na(surv))%>%print(n=4)
d$age=cut(d$agedx,c(0,40,60,80,126),include.lowest = T)
d$year=cut(d$yrdx,c(1975,1995,2016),include.lowest = T,dig.lab=4)
mkDemographics(d,outDir="uveal/outs")
d$cancer="uveal"
mkDemographics(d,outDir="uveal/outs")
