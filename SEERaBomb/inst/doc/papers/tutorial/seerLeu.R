graphics.off();rm(list=ls())  
library(SEERaBomb);library(tidyverse)
load("~/data/SEER/mrgd/cancDef.RData") 
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
leus=c("AML","ALL","CML")
d=canc%>%filter(cancer%in%leus)%>%mutate(age=agedx+0.5,year=yrdx)
(d=d%>%count(cancer,age,year))
load("~/data/SEER/mrgd/popsae.RData") 
(p=popsae%>%count(age,year,wt=py)%>%rename(py=n))
d=left_join(p,d)
d[is.na(d$n),"n"]=0
d=d%>%filter(age<=85,year>=2000)
d=d%>%mutate(ageg=cut(age,seq(0,85,5)))
d=d%>%group_by(cancer,ageg)%>%
  summarize(age=mean(age),py=sum(py)/1e5,n=sum(n))%>%
  mutate(incid=n/py,grp="Background")
d=d%>%select(cancer,grp,everything(),-ageg)#reorder columns
NHM=c("breast","thyroid","brain","renal")#NHM=non-heme malignancy
brksa=c(0,40,50,60,70,80)#broad 1st interval avoids 0 CML groups
system.time(ra<-riskVsAge(canc,firstS=NHM,secondS=leus,brksa=brksa))#~15s
raCML<-riskVsAge(canc,firstS=c("AML","ALL"),secondS="CML",brksa=brksa)
D=bind_rows(ra,raCML)
D=D%>%filter(rad!="Unk",chemo!="Unk")
D=D%>%group_by(cancer2,rad,chemo,age)%>%
  summarize(py=sum(py),n=sum(o),incid=n/py)
D=D%>%rename(cancer=cancer2)%>%unite(grp,rad,chemo,sep=", ")
dd=bind_rows(D,d)
ord=c("Rad, Chemo","No Rad, Chemo","Rad, No Chemo",
      "No Rad, No Chemo","Background")
dd$grp=factor(dd$grp,levels=ord)
dd$cancer=factor(dd$cancer,levels=c("AML","ALL","CML"))
myt=theme(legend.position=c(.25,.22),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height=unit(.65,'lines'))
ggplot(dd,aes(x=age,y=incid,col=grp))+geom_line()+myt+facet_grid(~cancer)+
  xlab("Attained Age (Years)")+scale_x_continuous(breaks=c(0,25,50,75))+
  ylab(expression(paste("Cases per ",10^5," Person-Years")))+
  scale_y_log10()+coord_cartesian(ylim=c(0.01,40))
ggsave("~/Results/tutorial/seerLeu.pdf",width=3.5,height=2.5)#Fig.6

# dd=dd%>%mutate(LL=qpois(0.025,n)/py,UL=qpois(0.975,n)/py) #look at some CI
# dd%>%filter(cancer=="CML",grp%in%c("Rad, Chemo","Rad, No Chemo"))%>%arrange(age)
# dd%>%filter(cancer=="AML",grp%in%c("Rad, Chemo","No Rad, Chemo"))%>%arrange(age)
# dd%>%filter(cancer=="ALL",grp%in%c("Rad, Chemo","No Rad, Chemo"))%>%arrange(age)
