graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
library(WriteXLS)
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
head(canc,2)
(codes=paste0("C",690:699))
d=canc%>%filter(primsite%in%codes,histo3%in%8720:8790) 
d$cancer=as.character(d$cancer)
# C69.3 (choroid), C69.4 (ciliary body and iris), and C69.2 (retina). 
d$cancer[d$primsite=="C690"]="C690-Conjunctiva"
d$cancer[d$primsite=="C691"]="C691-Cornea"
d$cancer[d$primsite=="C692"]="C692-Retinal"
d$cancer[d$primsite=="C693"]="C693-Choroid"
d$cancer[d$primsite=="C694"]="C694-Ciliary"
d$cancer[d$primsite=="C695"]="C695-Lacrimal"
d$cancer[d$primsite=="C696"]="C696-Orbital"
d$cancer[d$primsite=="C698"]="C698-Overlap"
d$cancer[d$primsite=="C699"]="C699-NOS"
(tb1=d%>%group_by(cancer)%>%summarize(cases=n())%>%mutate(percent=100*cases/sum(cases)))
tb1=add_row(tb1,cancer="Total",cases=sum(tb1$cases),percent=sum(tb1$percent))
names(tb1)=c("Cancer","Cases","Prct")
WriteXLS(tb1,ExcelFileName="uveal/outs/tb1.xlsx",AdjWidth=T)

dh=d%>%mutate(yrgrp=ifelse(yrdx<2014,"1975-2013","2014-2016"))
table(dh$yrgrp)
(tb2a=dh%>%group_by(cancer,yrgrp)%>%summarize(nO3=n())%>%mutate(propO3=nO3/sum(nO3)))
(tb2a=tb2a%>%select(-propO3)%>%spread(yrgrp,nO3))

(tb2b=dh%>%group_by(cancer,yrgrp,db)%>%summarize(nO3=n())%>%mutate(propO3=nO3/sum(nO3)))
(tb2b=tb2b%>%select(-propO3)%>%spread(db,nO3))
WriteXLS(list(Table2a=tb2a,Table2b=tb2b),ExcelFileName="uveal/outs/tb2.xlsx",AdjWidth=T)

########### toying below
d=canc%>%filter(cancer=="eye") #17480 # mapCancs # eye is ICD9 190.0 to 190.9
d$cancer=as.character(d$cancer)
d$cancer[d$primsite=="C690"]="Conjunctiva"
d$cancer[d$primsite=="C691"]="Cornea"
d$cancer[d$primsite=="C692"]="Retinal"
d$cancer[d$primsite=="C693"]="Choroid"
d$cancer[d$primsite=="C694"]="Ciliary"
d$cancer[d$primsite=="C695"]="Lacrimal"
d$cancer[d$primsite=="C696"]="Orbital"
d$cancer[d$primsite=="C698"]="Overlap"
d$cancer[d$primsite=="C699"]="NOS"
table(d$cancer) # lots of retinals
d=d%>%filter(histo3%in%8720:8790) 
table(d$cancer)  # all lost. Use SEER*stat to find out who they are


