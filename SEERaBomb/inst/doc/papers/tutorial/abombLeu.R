graphics.off();rm(list=ls())
library(tidyverse);library(SEERaBomb)
load("~/data/abomb/abomb.RData")
(d=heme%>%select(ageG:DG,age,agex,t,D,py,AML=AMLtot,ALL,CML))
(dA=incidAbomb(d%>%group_by(ageG,DG)))
myt=theme(legend.position=c(.52,.85),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height=unit(.7,'lines'))
gy=ylab(quote(paste("Cases per ",10^5," Person-Years")))
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)
g=ggplot(dA,aes(x=age,y=I,shape=DG,col=DG))+geom_point()+geom_line()+
 xlab("Attained Age (Years)")+scale_y_log10()+facet_wrap(~cancer)  
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuAge.pdf",width=4,height=2.5)#Fig.5A

(dB=incidAbomb(d%>%group_by(agexG,DG)))
g=ggplot(dB,aes(x=agex,y=I,shape=DG,col=DG))+geom_point()+geom_line()+
 xlab("Age at Time of Bombing (Years)")+scale_y_log10()+facet_wrap(~cancer)
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuAgex.pdf",width=4,height=2.5)#Fig.5B

(dC=incidAbomb(d%>%group_by(tG,DG)))
g=ggplot(dC,aes(x=t,y=I,shape=DG,col=DG))+geom_point()+geom_line()+
  xlab("Years Since Bombing")+scale_y_log10()+facet_wrap(~cancer)
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuTsx.pdf",width=4,height=2.5)#Fig.5C

d$DG<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
(dD=incidAbomb(d%>%group_by(DG)))
ggplot(dD,aes(x=D,y=I))+geom_point()+geom_line()+
  xlab("Dose (Sv)")+scale_y_log10()+facet_wrap(~cancer)+gy+ge
ggsave("~/Results/tutorial/abombLeuDoseResp.pdf",width=4,height=2.5)#Fig.5D


