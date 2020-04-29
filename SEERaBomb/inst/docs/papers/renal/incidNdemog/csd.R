graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci)
library(survival);library(survminer);library(bbmle)
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")
canc$cancer=as.character(canc$cancer)
canc[(canc$primsite%in%c("C649"))&(canc$histo3%in%8310),"cancer"]="ccRCC"  
secs=c("ccRCC")#second cancer of interest
pf=seerSet(canc,popsae,Sex="Female");pm=seerSet(canc,popsae,Sex="Male") 
pf=mk2D(pf,secondS=secs);pm=mk2D(pm,secondS=secs)
# plot2D(pf) # these stopped working with R4.0 so skip for now
# plot2D(pm)
mybrks=c(0,1,2,3,5,10)
pf=csd(pf,brkst=mybrks)
pm=csd(pm,brkst=mybrks)
(lab=paste0("b",paste(mybrks,collapse="_")))
mkExcelCsd(pf,lab,outDir="uveal/outs",outName="csdF",flip=T)
mkExcelCsd(pm,lab,outDir="uveal/outs",outName="csdM",flip=T)

DF=bind_rows(pf$DF,pm$DF)
D=DF%>%filter(cancer1!="ccRCC")%>%group_by(int)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D
source("uveal/common/acros.R")
gx=xlab("Years Since 1st Cancer Dx")
gy=ylab("Relative Risk of ccRCC")
D%>%ggplot(aes(x=t,y=RR))+gp+gl+gx+gy+geRR+tc(14)
ggsave("renal/outs/csd.pdf",width=4,height=3)


