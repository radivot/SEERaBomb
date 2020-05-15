graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci)
library(survival);library(survminer);library(bbmle)
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")
(codes=paste0("C",692:694))
canc$cancer=as.character(canc$cancer)
canc[(canc$primsite%in%codes)&(canc$histo3%in%8720:8790),"cancer"]="uveal"  

secs=c("uveal")#second cancer of interest
gp=geom_point();gl=geom_line()
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
gh=geom_hline(yintercept=1)
svts=scale_x_continuous(breaks=c(0,5,10))#surv times
agts=scale_x_continuous(breaks=c(25,50,75))#age times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
sy=scale_y_log10()
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
pf=seerSet(canc,popsae,Sex="Female");pm=seerSet(canc,popsae,Sex="Male") 
pf=mk2D(pf,secondS=secs);pm=mk2D(pm,secondS=secs)
# plot2D(pm)
# plot2D(pf)
mybrks=c(0,1,2,3,5,10)
pf=csd(pf,brkst=mybrks)
pm=csd(pm,brkst=mybrks)
DF=bind_rows(pf$DF,pm$DF)
D=DF%>%group_by(int,rad)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D=D%>%filter(rad!="Unk")
D
gx=xlab("Years Since 1st Cancer Diagnosis")
gy=ylab("Relative Risk of Uveal Cancer")
D%>%ggplot(aes(x=t,y=RR,col=rad))+ 
  gp+gl+gx+gy+gh+geRR+tc(14)+jco+sbb+ltb+ltp+lh
ggsave("uveal/outs/csdRadvsNot.pdf",width=4,height=3)

(lab=paste0("b",paste(mybrks,collapse="_")))
mkExcelCsd(pf,lab,outDir="uveal/outs",outName="csdF",flip=T)
mkExcelCsd(pm,lab,outDir="uveal/outs",outName="csdM",flip=T)

D=DF%>%filter(cancer1!="uveal")%>%group_by(int,rad)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D=D%>%filter(rad!="Unk")
D
gx=xlab("Years Since 1st Non-Uveal Dx")
gy=ylab("Relative Risk of Uveal Cancer")
D%>%ggplot(aes(x=t,y=RR,col=rad))+ 
  gp+gl+gx+gy+gh+geRR+tc(14)+jco+sbb+ltb+ltp+lh
ggsave("uveal/outs/csdRadvsNotAndNotUV.pdf",width=4,height=3)


