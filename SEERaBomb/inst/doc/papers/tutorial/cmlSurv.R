graphics.off()  #clear all plots
load("~/data/SEER/mrgd/cancDef.RData") #loads canc
d=canc%>%filter(cancer=="CML",surv<9999) #remove unknown survival cases
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),dig.lab = 4))
d=d%>%select(yrg,agedx,sex,surv,status)
library(survival)
fit <- survfit(Surv(surv, status) ~ yrg + sex,data=d) 
library(survminer)
quartz(width=4.5,height=3)
ggsurvplot_facet(fit,d,facet.by=c("sex"), legend.title="Years",
                 xlab="Years Since CML Diagnosis", 
                 xlim=c(0,12), short.panel.labs=T,
                 ylab="Survival Probability",
                 legend.labs=levels(d$yrg))+
  scale_x_continuous(breaks=seq(0,15,5))
ggsave("~/Results/tutorial/CMLsurvTrends.pdf")  



