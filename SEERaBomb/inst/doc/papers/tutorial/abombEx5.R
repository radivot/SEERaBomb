#abombEx5.R
(d=heme%>%select(ageG:DG,age,agex,t,D,py,AML=AMLtot,ALL,CML))
myt=theme(legend.position=c(.52,.85),legend.key.height=unit(.7,'lines'))
cc=coord_cartesian(ylim=c(.2,200));f=facet_wrap(~cancer)

gx=xlab("Attained Age (Years)")
(dA=incidAbomb(d%>%group_by(ageG,DG)))
ggplot(dA,aes(x=age,y=I,col=DG))+gp+gl+gx+sy+gyi+ge+f+cc+tc(12)+myt+jco+sbb+ltb
ggsave("~/Results/tutorial/abombLeuAge.pdf",width=4,height=2.5)#Fig.5A

gx=xlab("Age at Time of Bombing (Years)")
(dB=incidAbomb(d%>%group_by(agexG,DG)))
ggplot(dB,aes(x=agex,y=I,col=DG))+gp+gl+gx+sy+gyi+ge+f+cc+tc(12)+myt+jco+sbb+ltb
ggsave("~/Results/tutorial/abombLeuAgex.pdf",width=4,height=2.5)#Fig.5B

gx=xlab("Years Since Bombing")
(dC=incidAbomb(d%>%group_by(tG,DG)))
ggplot(dC,aes(x=t,y=I,col=DG))+gp+gl+gx+sy+gyi+ge+f+cc+tc(12)+myt+jco+sbb+ltb
ggsave("~/Results/tutorial/abombLeuTsx.pdf",width=4,height=2.5)#Fig.5C

gx=xlab("Dose (Sv)")
d$DG<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
(dD=incidAbomb(d%>%group_by(DG)))
ggplot(dD,aes(x=D,y=I))+gp+gl+gx+sy+gyi+ge+f+cc+tc(12)+myt+jco+sbb+ltb+
  scale_x_continuous(breaks=0:2)
ggsave("~/Results/tutorial/abombLeuDoseResp.pdf",width=4,height=2.5)#Fig.5D


