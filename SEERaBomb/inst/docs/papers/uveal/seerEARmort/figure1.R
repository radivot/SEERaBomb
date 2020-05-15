graphics.off();rm(list=ls())#clear plots and environment
load("uveal/data/allfields2019.RData")
library(dplyr)
d$CODS=as.character(d$CODS)
d=d%>%mutate(CODS=ifelse(!CODS%in%c("alive","melanoma","eye"),"other",CODS))
d$CODS[d$CODS=="eye"]="melanoma"
#Figure 1
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%mutate(survG=cut(surv,c(0,5,10,15,20,25,30,45),
                       dig.lab=4,include.lowest=T,right=F))
m=d
table(m$survG,m$CODS) # make this in dplyr
library(tidyr)
(fg1=m%>%group_by(survG,CODS)%>%summarize(n=n()))
(fg1=fg1%>%spread(CODS,n))
# (fg1=fg1%>%mutate(n=other+melanoma))
# (fg1=fg1%>%mutate(frc=other/n))
# fg1%>%group_by()%>%summarize(M=sum(melanoma),O=sum(other),P=O/(M+O))
library(WriteXLS)
WriteXLS(list(Fig1=fg1),ExcelFileName="uveal/outs/fig1.xlsx")
# fraction of deaths by UVM in all vs SEER9
(tb=table(d$CODS))
sum(tb[2:3])
tb[2]/sum(tb[2:3])
d9=d%>%filter(db=="73")
(tb=table(d9$CODS))
sum(tb[2:3])
tb[2]/sum(tb[2:3])
table(d$db)
######### skip below

# 
# 
# 
# 
# range(d$surv)
# 
# d=d%>%mutate(yrgrp=cut(yrdx,c(1975,1983,1993,2003,2016),
#                        dig.lab=4,include.lowest=T,right=F)) 
# table(d$yrgrp)
# names(d)
# (tb=table(d$trt))
# # (tb=table(d$nosurg,d$trt)) # surg field is a nightmare
# # (tb=table(d$nosurg))
# # (tb=table(d$radsurg))
# d$surv
# table(d$CODS)
