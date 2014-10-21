rm(list=ls()) 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsa.RData")
head(canc)
head(popsa)
sSetL=seerSet(canc,popsa,picks=c("CML","CMML","MDS","AML","CLL","ALL"))
str(sSetL)
pmL=mk2D(sSetL,txt="leuks") # pooled race male leukemias 
plot2D(pmL)

canc%>%filter(cancer=="MDS")%>%group_by(yrdx)%>%summarise(count=n())
canc%>%filter(cancer=="CMML")%>%group_by(yrdx)%>%summarise(count=n())
