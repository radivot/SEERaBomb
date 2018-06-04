graphics.off();rm(list=ls())
library(SEERaBomb)#version 2018.1 or higher is needed for this tutorial
load("~/data/SEER/mrgd/cancDef.RData")
d=canc%>%filter(cancer=="CML")%>%print(n=13)
d%>%summarize(n=n(),na=sum(is.na(surv)),prct=100*na/n)#<2% missing
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)#move into mkSEER?
d=d%>%select(yrdx,agedx,sex,surv,status)%>%print(n=13)
load("~/data/usMort/mrt.RData")#loads mrt
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1973,1990,2005,2015)))
(g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"),facets=.~sex,
        xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality"))
library(ggsci);(g=g+scale_color_jco()+theme_classic(base_size = 14))
(g=g+scale_x_continuous(breaks=seq(0,15,5)))
(g=g+theme(legend.position="top",legend.title=element_blank(),
           strip.background=element_blank()))
(g=g+geom_abline(intercept=1,slope=0)+ylim(c(0,NA)))
g+geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
ggsave("~/Results/tutorial/CMLmortRR.pdf",width=4.5,height=3)

labs=c("1973-1990","1991-2005","2006-2015")
d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),labels=labs))%>%print(n=13)
library(survival);library(survminer)
fit=survfit(Surv(surv,status)~yrg+sex,data=d) 
ggsurvplot_facet(fit,d,facet.by=c("sex"),ylab="Survival Probability",
                 xlab="Years Since CML Diagnosis",legend.title="",
                 xlim=c(0,12),short.panel.labs=T)+
  scale_x_continuous(breaks=seq(0,15,5))+
  scale_color_jco()+theme(strip.background=element_blank())
ggsave("~/Results/tutorial/CMLsurvTrends.pdf",width=4.5,height=3)



