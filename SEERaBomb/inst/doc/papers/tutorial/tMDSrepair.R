graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse)          #load R packages dplyr and ggplot2 
load("~/data/SEER/mrgd/cancDef.RData")
(sc=canc%>%filter(histo3==9920|histo3==9987,yrdx>2000))#subset canc
(d=sc%>%count(sex,yrdx,histo3))#n in d holds sex-year-ICDO3 group row counts 
(g=qplot(yrdx,n,data=d,facets=sex~histo3,ylab="Cases",xlab="Year of Diagnosis"))
g+scale_x_continuous(breaks=seq(2005,2015,5))
dir.create("~/Results/tutorial",recursive=T)
ggsave("~/Results/tutorial/tMDSrepaired.pdf",width=3,height=2.5) 
# quartz(width=3,height=2.5);g;ggsave("~/Results/tutorial/tMDSrepaired.pdf") 
# options(device = "RStudioGD")