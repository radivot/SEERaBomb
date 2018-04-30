graphics.off();rm(list=ls())
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
d=canc%>%filter(cancer=="CML")
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)
load("~/data/usMort/mrt.RData")#loads mrt
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1973,1990,2005,2015)))
(g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"),facets=.~sex,
        xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality"))
(g=g+scale_x_continuous(breaks=seq(0,15,5)))
(g=g+theme(legend.position="top",legend.title=element_blank()))
(g=g+geom_abline(intercept=1,slope=0)+ylim(c(0,NA)))#ylim()=>include y=0
(g=g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15)))
ggsave("~/Results/tutorial/CMLmortRR.pdf",width=4.5,height=3)

(d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),dig.lab=4)))
d$yrg=c("1973-1990","1991-2005","2006-2015")[d$yrg]
(d=d%>%filter(surv<200))
library(survival)
fit=survfit(Surv(surv,status)~yrg+sex,data=d) 
library(survminer)
ggsurvplot_facet(fit,d,facet.by=c("sex"),legend.title="",
                 xlab="Years Since CML Diagnosis", 
                 xlim=c(0,12),short.panel.labs=T,
                 ylab="Survival Probability")+
  scale_x_continuous(breaks=seq(0,15,5))
ggsave("~/Results/tutorial/CMLsurvTrends.pdf",width=4.5,height=3)



