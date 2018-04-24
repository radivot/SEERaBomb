rm(list=ls())
library(tidyverse)
load("~/data/abomb/abomb.RData"); head(heme,3)
d=heme%>%mutate(t=year-1945.6)  # t is used in Fig 5C
(d=d%>%select(age,agex,t,D,py,AML=AMLtot,ALL,CML))#agex is used in 5B
d$Dose<-cut(d$D,c(-1,.02,1,100),labels=c("Ctrl","<1Sv",">1Sv"))
d$Dose=factor(d$Dose,levels=c(">1Sv","<1Sv","Ctrl"))#so >1Sv at legend top
d$ageG=cut(d$age,c(0,20,40,60,70,80,110))
dA=d%>%group_by(Dose,ageG) # for subplot A
(ds=dA%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dA%>%summarize_at(vars(age,D),funs(weighted.mean(.,w=py))))
joinM=function(dm,ds) {    #define function for code reuse below
  D=left_join(dm,ds)#like bind_cols()/cbind() but removes exta col copies 
  D=D%>%gather(value="O",key="leuk",-(Dose:py)) #O for Observed cases
  D$leuk=factor(D$leuk,levels=c("AML","ALL","CML"))#sets this plot order
  D=D%>%mutate(py=py/1e5,I=O/py,LL=qpois(0.025,O)/py,UL=qpois(0.975,O)/py)
  D # last unassigned variable is returned. Same as return(D)
}
(D=joinM(dm,ds)) 
graphics.off();quartz(width=4,height=2.5)
myt=theme(legend.position=c(.6, .85),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height = unit(.7, 'lines'))
g=ggplot(D,aes(x=age,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
 xlab("Attained Age (Years)")+scale_y_log10() + facet_wrap(~leuk)+
 ylab(expression(paste("Cases per ",10^5," Person-Years"))) + myt   
g+geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)+coord_cartesian(ylim=c(.2,120))
ggsave("~/Results/tutorial/abombLeuAge.pdf") # Fig 5A

d$agexG=cut(d$agex,c(0,20,40,110))
dB=d%>%group_by(Dose,agexG) # for subplot B
(ds=dB%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dB%>%summarize_at(vars(agex,D),funs(weighted.mean(.,w=py))))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=agex,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
 xlab("Age at Time of Bombing (Years)")+scale_y_log10()+facet_wrap(~leuk)+
 ylab(expression(paste("Cases per ",10^5," Person-Years"))) + myt   
g+geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)+coord_cartesian(ylim=c(.2,120))
ggsave("~/Results/tutorial/abombLeuAgex.pdf") # Fig 5B

d$tG=cut(d$t,c(0,10,20,40,110)) 
dC=d%>%group_by(Dose,tG) # for subplot C
(ds=dC%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dC%>%summarize_at(vars(t,D),funs(weighted.mean(.,w=py))))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=t,y=I,shape=Dose,col=Dose))+geom_point()+geom_line()+
  xlab("Years Since Bombing")+scale_y_log10()+facet_wrap(~leuk)+
  ylab(expression(paste("Cases per ",10^5," Person-Years"))) + myt   
g+geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)+coord_cartesian(ylim=c(.2,120))
ggsave("~/Results/tutorial/abombLeuTsx.pdf") # Fig 5C

d$Dose<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
dD=d%>%group_by(Dose) # for subplot D
(ds=dD%>%summarize_at(vars(py:CML),funs(sum)))
(dm=dD%>%summarize(D=weighted.mean(D,w=py)))
(D=joinM(dm,ds))
g=ggplot(D,aes(x=D,y=I))+geom_point()+geom_line()+
  xlab("Dose (Sv)")+scale_y_log10()+facet_wrap(~leuk)+
  ylab(expression(paste("Cases per ",10^5," Person-Years"))) 
g+geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1) 
ggsave("~/Results/tutorial/abombLeuDoseResp.pdf") # Fig 5D


