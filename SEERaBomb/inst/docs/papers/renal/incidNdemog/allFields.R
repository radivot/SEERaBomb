graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
system.time(load("~/data/SEER/mrgd/cancALL.RData")) # which takes ~65 secs
d=canc%>%filter(primsite%in%c("C649"),histo3==8310)
nms=names(d)
L=lapply(nms,function(x) {dd=as.data.frame(table(d[,x]))
                          if (dim(dd)[2]==2) names(dd)=c("Variable","Frequency")
                          dd} )
names(L)=nms
L
library(WriteXLS)
WriteXLS(L,ExcelFileName="renal/outs/ccRCCallFields.xlsx",AdjWidth=T)
