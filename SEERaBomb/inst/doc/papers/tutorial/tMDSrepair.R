rm(list=ls())
library(tidyverse)
load("~/data/SEER/mrgd/cancDef.RData") #loads canc
sc=canc%>%filter(histo3==9920|histo3==9987,yrdx>2000) #tAML and tMDS  subset of canc (sc)
d=sc%>%group_by(sex,yrdx,histo3)%>%summarize(cnt=n()) #count rows in sex-year-ICDO3 groups
theme_update(axis.text=element_text(size=rel(1.2)),
             axis.title=element_text(size=rel(1.3)),
             strip.text = element_text(size = rel(1.5)))
if(.Platform$OS.type=="windows") quartz<-function() windows() 
quartz(width=5,height=4) 
qplot(yrdx,cnt,data=d,facets=sex~histo3,ylab="Cases",xlab="Year of Diagnosis")+
  scale_x_continuous(breaks=seq(2002,2016,4))
ggsave("~/Results/amlMDS/tAMLtMDSfixed.pdf")  

