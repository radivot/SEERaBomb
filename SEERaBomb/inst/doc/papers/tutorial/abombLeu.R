rm(list=ls())
library(tidyverse)
load("~/data/abomb/abomb.RData")
head(heme)
(d=heme%>%mutate(t=year-1945.6))
(d=d%>%select(age,agex,t,D,py,AML=AMLtot,ALL,CML))
d$Dose<-cut(d$D,c(-1,.02,1,100),labels=c("Ctrl","<1Sv",">1Sv"))
d$ageG=cut(d$age,c(0,20,40,60,70,80,110))
d=d%>%group_by(Dose,ageG)
(ds=d%>%summarize_at(vars(py:CML),sum))
(dm=d%>%summarize_at(vars(age:D),funs(weighted.mean(.,w=py))))
(D=left_join(dm,ds))
(D=D%>%gather(value="O",key="leuk",-(Dose:py)))
D=D%>%mutate(py=py/1e5,I=O/py,LL=qpois(0.025,O)/py,UL=qpois(0.975,O)/py)
graphics.off();quartz(width=4,height=2.5)
myt=theme(legend.position=c(.25, .85),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height = unit(.7, 'lines'))
D$Dose=factor(D$Dose,levels=c(">1Sv","<1Sv","Ctrl"))#high to legend top
g=ggplot(D,aes(x=age,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
  xlab("Attained-age (PY-weighted)")+scale_y_log10() + facet_wrap(~leuk)+
  ylab(expression(paste("Cases per ",10^5," Person-Years"))) + myt+   
g+geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)+coord_cartesian(ylim=c(.1,100))
ggsave("~/Results/tutorial/abombLeu.pdf") # Fig 6A

