graphics.off(); rm(list=ls())  #clear plots and environment
library(tidyverse) # load R packages dplyr and ggplot2 
load("~/data/SEER/mrgd/cancDef.RData") #load canc 
(sc=canc%>%filter(histo3==9920|histo3==9987,yrdx>2000)) #sc = subset of canc
(d=sc%>%count(sex,yrdx,histo3)) #output col n = number in sex-year-ICDO3 group 
if(.Platform$OS.type=="windows") quartz<-function() windows() 
quartz(width=3,height=2.5) # line above => this works on Windows
qplot(yrdx,n,data=d,facets=sex~histo3,ylab="Cases",xlab="Year of Diagnosis") +
  scale_x_continuous(breaks=seq(2005,2015,5)) #+theme_bw()
ggsave("~/Results/tutorial/tMDSrepaired.pdf")  

