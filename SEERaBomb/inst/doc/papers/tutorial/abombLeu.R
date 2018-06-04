rm(list=ls())
library(SEERaBomb);library(ggsci)
myt=theme(legend.position=c(.52,.85),legend.title=element_blank(),
    legend.direction="vertical",legend.margin=margin(0,0,0,0),
    legend.key.height=unit(.7,'lines'),strip.background=element_blank())
gy=ylab(quote(paste("Cases per ",10^5," Person-Years")))
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1);jco=scale_color_jco()
cc=coord_cartesian(ylim=c(.2,200));tc=theme_classic()
gp=geom_point();gl=geom_line();f=facet_wrap(~cancer);sy=scale_y_log10()
load("~/data/abomb/abomb.RData")
(d=heme%>%select(ageG:DG,age,agex,t,D,py,AML=AMLtot,ALL,CML))

gx=xlab("Attained Age (Years)")
(dA=incidAbomb(d%>%group_by(ageG,DG)))
ggplot(dA,aes(x=age,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/tutorial/abombLeuAge.pdf",width=4,height=2.5)#Fig.6A

gx=xlab("Age at Time of Bombing (Years)")
(dB=incidAbomb(d%>%group_by(agexG,DG)))
ggplot(dB,aes(x=agex,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/tutorial/abombLeuAgex.pdf",width=4,height=2.5)#Fig.6B

gx=xlab("Years Since Bombing")
(dC=incidAbomb(d%>%group_by(tG,DG)))
ggplot(dC,aes(x=t,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/tutorial/abombLeuTsx.pdf",width=4,height=2.5)#Fig.6C

gx=xlab("Dose (Sv)")
d$DG<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
(dD=incidAbomb(d%>%group_by(DG)))
ggplot(dD,aes(x=D,y=I))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/tutorial/abombLeuDoseResp.pdf",width=4,height=2.5)#Fig.6D


