rm(list=ls()) 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsae.RData")
pm=seerSet(canc,popsae)
pm=mk2D(pm,secondS=c("CML","CMML","MDS","AML","CLL","ALL")) # pooled race male leukemias 
plot2D(pm)

canc%>%filter(cancer=="MDS")%>%group_by(yrdx)%>%summarise(count=n())
canc%>%filter(cancer=="CMML")%>%group_by(yrdx)%>%summarise(count=n())
