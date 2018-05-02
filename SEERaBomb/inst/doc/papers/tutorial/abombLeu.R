graphics.off();rm(list=ls())
library(tidyverse)
load("~/data/abomb/abomb.RData")
(d=heme%>%select(age,agex,t,D,py,AML=AMLtot,ALL,CML))
d$Dose<-cut(d$D,c(-1,.02,1,100),labels=c("Ctrl","<1Sv",">1Sv"))
d$Dose=factor(d$Dose,levels=c(">1Sv","<1Sv","Ctrl"))#>1Sv at top
d$ageG=cut(d$age,c(0,20,40,60,70,80,110))
dA=d%>%group_by(Dose,ageG)
(ds=dA%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dA%>%summarize_at(vars(age,D),funs(weighted.mean(.,w=py))))
joinM=function(dm,ds) {
  D=left_join(dm,ds)#like bind_cols() but removes exta col copies 
  D=D%>%gather(value="O",key="leuk",-(Dose:py))#O=Observed
  D$leuk=factor(D$leuk,levels=c("AML","ALL","CML"))#set order
  D%>%mutate(py=py/1e5,I=O/py,LL=qpois(0.025,O)/py,UL=qpois(0.975,O)/py)
}
(D=joinM(dm,ds)) 
myt=theme(legend.position=c(.52,.85),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height=unit(.7,'lines'))

gy=ylab(quote(paste("Cases per ",10^5," Person-Years")))
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)
g=ggplot(D,aes(x=age,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
 xlab("Attained Age (Years)")+scale_y_log10()+facet_wrap(~leuk)  
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuAge.pdf",width=4,height=2.5)#Fig.5A

d$agexG=cut(d$agex,c(0,20,40,110))
dB=d%>%group_by(Dose,agexG)
(ds=dB%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dB%>%summarize_at(vars(agex,D),funs(weighted.mean(.,w=py))))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=agex,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
 xlab("Age at Time of Bombing (Years)")+scale_y_log10()+facet_wrap(~leuk)
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuAgex.pdf",width=4,height=2.5)#Fig.5B

d$tG=cut(d$t,c(0,10,20,40,110)) 
dC=d%>%group_by(Dose,tG)
(ds=dC%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dC%>%summarize_at(vars(t,D),funs(weighted.mean(.,w=py))))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=t,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
  xlab("Years Since Bombing")+scale_y_log10()+facet_wrap(~leuk)
g+gy+ge+coord_cartesian(ylim=c(.2,200))+myt
ggsave("~/Results/tutorial/abombLeuTsx.pdf",width=4,height=2.5)#Fig.5C

d$Dose<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
dD=d%>%group_by(Dose)
(ds=dD%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dD%>%summarize(D=weighted.mean(D,w=py)))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=D,y=I))+geom_point()+geom_line()+
  xlab("Dose (Sv)")+scale_y_log10()+facet_wrap(~leuk)+gy+ge
ggsave("~/Results/tutorial/abombLeuDoseResp.pdf",width=4,height=2.5)#Fig.5D


