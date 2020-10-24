graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
library(writexl)
(df=getFields("~/data/SEER")) # this doesn't fly in windows since ~ maps to /users/radivot/documents
write_xlsx(df,path="pmf/outs/fieldDefs2019.xlsx") # key to big file above
# picks=c("casenum","reg","race","sex","agedx",
#         "yrbrth","seqnum","modx","yrdx","histo3",
#         "ICD9","primsite","COD","surv","radiatn","chemo")
# (rdf=pickFields(df,picks))
# mkSEER(rdf,outFile="cancPrim",writePops=F) 
# rdf=pickFields(df,picks=df$names)
# mkSEER(rdf,outFile="cancAll",writePops=F) #149 secs # runs out of mem (fixed via more virt memory)
# system.time(load("~/data/SEER/mrgd/cancALL.RData")) # which takes ~65 secs
# d=canc%>%filter(histo3==9961)%>%print(n=2)
# table(d$primsite) # only 4 cases outside of bone = C421
# d=d%>%filter(primsite%in%c("C421"))%>%print(n=2) #4458 cases
# d=d%>%filter(!is.na(surv))%>%print(n=4)  # 4453 with surv times
# save(d,file="pmf/data/allfields2019.RData")
load("pmf/data/allfields2019.RData")
nms=names(d)
L=lapply(nms,function(x) {dd=as.data.frame(table(d[,x],useNA="always"))
                          if (dim(dd)[2]==2) names(dd)=c("Variable","Frequency")
                          dd} )
names(L)=nms
write_xlsx(L,path="pmf/outs/AllFieldsNA.xlsx") # key to big file above

table(d$seqnum,d$yrdx)  # flare up in 2nd cancers in 2010 and 2012
table(d$dxconf,d$yrdx)  # flare up in code 3 in   2010 and 2012 
# (3 = immuno and genotype additional confirmation)
sort(table(d$COD))  # COD=3800=unknown behavior neoplasm is biggest at 1106

