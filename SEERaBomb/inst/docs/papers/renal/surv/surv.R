source("renal/common/load.R")
source("renal/common/acros.R")

(d5=d%>%select(yrdx,agedx,sex,surv,status))
(d6=d%>%select(yrdx,agedx,sex,surv,status,CODS))
(d6=d6%>%mutate(status=ifelse((status==1)&(!CODS%in%c("renal")),0,status))%>%select(-CODS))
d5$grp="All Cause Mortality"
d6$grp="Death by RCC"

D=bind_rows(d5,d6)
fit=survfit(Surv(surv,status)~grp,data=D) 
gy=ylab("Survival Probability")
labs=c("OS","MFS")
ggsurvplot(fit,D,legend.title="",legend.labs=labs)+gx
ggsave("renal/outs/survOSnMFS.pdf",width=4.5,height=3)

ddT=simSurv(d,mrt,rep=3,ltb=NULL,unif=TRUE)
fit=survfit(Surv(surv,status)~type,data=ddT) 
ggsurvplot(fit,ddT,legend.title="",legend.labs=c("Observed","Expected"),xlim=c(0,60))+gx #+svts
ggsave("renal/outs/survWithCntrl.png",width=4.5,height=3)

library("MALDIquant")
ddF=simSurv(d,mrt,rep=1,ltb=NULL,unif=FALSE)
fitd=survfit(Surv(surv,status)~1,data=ddF%>%filter(type=="Observed")) 
fitc=survfit(Surv(surv,status)~1,data=ddF%>%filter(type=="Simulated")) 
(tc=fitc$time[1:40])
(svc=fitc$surv[1:40])
(td=fitd$time)
(svd=fitd$surv)
(k=match.closest(tc,td))
svd=svd[k]
tibble(tc,svr=svd/svc)%>%ggplot(aes(tc,svr))+geom_step()+gx+gy
# bit crude looking

fitd=survfit(Surv(surv,status)~1,data=ddT%>%filter(type=="Observed")) 
fitc=survfit(Surv(surv,status)~1,data=ddT%>%filter(type=="Simulated")) 
x=0:40
x=seq(0,30,.1)
x=seq(0,42,.1)
(tc=fitc$time)
(k=match.closest(x,tc))
(svc=fitc$surv[k])
(td=fitd$time)
(k=match.closest(x,td))
(svd=fitd$surv[k])
gy=ylab("Relative Survival Probability")
tibble(x,svr=svd/svc)%>%ggplot(aes(x,svr))+geom_step()+gx+gy
ggsave("renal/outs/survRel.pdf",width=4.5,height=3)
ggsave("renal/outs/survRel.png",width=4.5,height=3)

