# extends cmlMort.R to 3x2 AML-ALL-CML tables of plots
graphics.off();rm(list=ls())
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
leus=c("AML","ALL","CML")
d=canc%>%filter(cancer%in%leus)%>%mutate(cancer=factor(cancer,leus))
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
(d=d%>%select(yrdx,agedx,sex,surv,status,cancer)%>%group_by(cancer))
load("~/data/usMort/mrt.RData")#loads mrt
(D=d%>%do(msd(.,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1973,1990,2005,2015)))) 
(g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"),facets=sex~cancer,
        xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality"))
(g=g+scale_x_continuous(breaks=seq(0,15,5))+scale_color_jco())
(g=g+theme(legend.position="top",legend.title=element_blank()))
(g=g+geom_abline(intercept=1,slope=0)+ylim(c(0,NA)))
g+geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
ggsave("~/Results/tutorial/mortRR.pdf",width=4.5,height=3)

labs=c("1973-1990","1991-2005","2006-2015")
d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),labels=labs))%>%print(n=13)
library(survival);library(survminer)
levels(d$cancer)
fit=survfit(Surv(surv,status)~yrg+sex+cancer,data=d) 
levels(d$cancer)
ggsurvplot_facet(fit,d,facet.by=c("sex","cancer"),ylab="Survival Probability",
                 xlab="Years Since CML Diagnosis",legend.title="",
                 xlim=c(0,12),short.panel.labs=T)+
  scale_x_continuous(breaks=seq(0,15,5))+scale_color_jco()
ggsave("~/Results/tutorial/leuSurvTrends.pdf",width=4.5,height=3)




