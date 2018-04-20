rm(list=ls())
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData") #loads in canc

sc=canc%>%filter(histo3==9920|histo3==9987) #tAML and tMDS  subset of cancs (sc)
d=sc%>%filter(yrdx>2000)%>%group_by(sex,yrdx,histo3)%>%summarize(cnt=n()) #count rows in sex-year-ICDO3 groups
library(ggplot2)
theme_update(axis.text=element_text(size=rel(1.2)),
             axis.title=element_text(size=rel(1.3)),
             strip.text = element_text(size = rel(1.5)))
qplot(yrdx,cnt,data=d,facets=sex~histo3,ylab="Cases",xlab="Year of Diagnosis")+
  scale_x_continuous(breaks=seq(2002,2015,4))
ggsave("~/Results/amlMDS/tAMLtMDSfixed.pdf")  

