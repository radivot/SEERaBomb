graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(WriteXLS)
(df=getFields("~/data/SEER")) # this doesn't fly in windows since ~ maps to /users/radivot/documents
WriteXLS(df,ExcelFileName="uveal/outs/fieldDefs2019.xlsx") # key to big file above
# picks=c("casenum","reg","race","sex","agedx",
#         "yrbrth","seqnum","modx","yrdx","histo3",
#         "ICD9","primsite","COD","surv","radiatn","chemo")
# (rdf=pickFields(df,picks))
# mkSEER(rdf,outFile="cancPrim",writePops=F) 
# rdf=pickFields(df,picks=df$names)
# mkSEER(rdf,outFile="cancAll",writePops=F) #149 secs # runs out of mem (fixed via more virt memory)

# system.time(load("~/data/SEER/mrgd/cancALL.RData")) # which takes ~65 secs
# d=canc%>%filter(primsite%in%c("C693","C694","C692"))%>%print(n=2)
# d=d%>%filter(histo3%in%8720:8790)%>%print(n=2)
# d=d%>%filter(!is.na(surv))%>%print(n=4)
# save(d,file="uveal/data/allfields2019.RData")
load("uveal/data/allfields2019.RData")
nms=names(d)
L=lapply(nms,function(x) {dd=as.data.frame(table(d[,x],useNA="always"))
                          if (dim(dd)[2]==2) names(dd)=c("Variable","Frequency")
                          dd} )
names(L)=nms
WriteXLS(L,ExcelFileName="uveal/outs/uvealAllFieldsNA.xlsx",AdjWidth=T)
# scrap yard below
# tb=tibble(siz=d$cstumsiz,stg=d$dajcc7t,ext=d$csexten,stg2=d$dajccstg)
# tb%>%ggplot(aes(x=ext,y=siz))+geom_point() # lots of missing sizes
# tb%>%ggplot(aes(x=stg,y=siz))+geom_jitter()
# tb%>%ggplot(aes(x=stg,y=stg2))+geom_jitter()
# plot(d$cstumsiz,d$csexten)
# plot(d$cstumsiz,d$dajcc7t)
# ### conclusion: there is no good tumor size metric, just  stages dajcc7t
# 

