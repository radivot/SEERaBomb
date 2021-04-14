library("MALDIquant")
library(WriteXLS)
source("uveal/common/setup.R")
(d6=d%>%select(yrdx,agedx,sex,surv,status,CODS))
d6=d6%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),0,status))
(d6=d6%>%select(-CODS))
d5$grp="All Cause Mortality"
d6$grp="Death by Melanoma"
D=bind_rows(d5,d6)
fit=survfit(Surv(surv,status)~grp,data=D) 
gy=ylab("Survival Probability")
labs=c("OS","MFS")
agts=scale_x_continuous(breaks=seq(0,40,5))#age times
ggsurvplot(fit,D,legend.title="",legend.labs=labs)+gx
ggsave("uveal/outs/survOSnMFS.pdf",width=4.5,height=3) #Fig. 2
s=summary(fit)
round(c(tail(s$surv)[6],tail(s$lower,1),tail(s$upper,1)),3) 
# # #so MFS yields 60% (0.584 0.629) # no sex diffs commented
# fitS=survfit(Surv(surv,status)~grp+sex,data=D)
# labs=c("OSmale","OSfem","MFSmale","MFSfemale")
# ggsurvplot(fitS,D,legend.title="",legend.labs=labs)+gx
# ggsave("uveal/outs/survOSnMFSbySex.pdf",width=4.5,height=3)
tb=tibble(t=s$time,P=s$surv,LL=s$lower,UL=s$upper,grp=s$strata)
table(tb$grp)
tbOS=tb%>%filter(grp=="grp=All Cause Mortality")
tbMFS=tb%>%filter(grp=="grp=Death by Melanoma")
tos=tbOS$t
tmfs=tbMFS$t
x=seq(5,35,5)
(kos=match.closest(x,tos))
mkCI=function(P,LL,UL) 
  paste0(sprintf('%.3f',P)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")")
(tbOSk=tbOS[kos,]%>%mutate(CI=mkCI(P,LL,UL)))
(kmfs=match.closest(x,tmfs))
(tbMFSk=tbMFS[kmfs,]%>%mutate(CI=mkCI(P,LL,UL)))
L=list(KMtable=tibble(Time=x,
                      "KM OS Probability"=tbOSk$CI,
                      "KM MFS Probability"=tbMFSk$CI
                      ))
WriteXLS(L,ExcelFileName="uveal/outs/KMestimates.xlsx",AdjWidth = T) #table 2
tbMFSk%>%mutate(mCI=mkCI(1-P,1-LL,1-UL)) #1-MFS values for text

ddT=simSurv(d,mrt,rep=3,ltb=NULL,unif=TRUE)  # takes several seconds
fit=survfit(Surv(surv,status)~type,data=ddT) 
labs=c("Observed","Expected")
ggsurvplot(fit,ddT,legend.title="",legend.labs=labs,xlim=c(0,60))+gx
ggsave("uveal/outs/survWithCntrl.pdf",width=4.5,height=3)# Fig. 4

# ddF=simSurv(d,mrt,rep=3,ltb=NULL,unif=FALSE)
# # ddF=simSurv(d,mrt,rep=30,ltb=NULL,unif=FALSE) 
# # 30 reps => no diff in  instability=> prob not in control group
# fitd=survfit(Surv(surv,status)~1,data=ddF%>%filter(type=="Observed")) 
# fitc=survfit(Surv(surv,status)~1,data=ddF%>%filter(type=="Simulated")) 
# (tc=fitc$time[1:40])
# (svc=fitc$surv[1:40])
# (td=fitd$time)
# (svd=fitd$surv)
# (k=match.closest(tc,td))
# svd=svd[k]
# tibble(tc,svr=svd/svc)%>%ggplot(aes(tc,svr))+geom_step()+gx+gy
# # bit crude looking

fitd=survfit(Surv(surv,status)~1,data=ddT%>%filter(type=="Observed"))#d=data
fitc=survfit(Surv(surv,status)~1,data=ddT%>%filter(type=="Simulated"))#c=control
x=seq(0,42,.1)
(tc=fitc$time)
(kc=match.closest(x,tc))
Dc=tibble(time=fitc$time,surv=fitc$surv,LL=fitc$lower,UL=fitc$upper)%>%slice(kc)
# (svc=fitc$surv[kc])

(td=fitd$time)
(kd=match.closest(x,td))
Dd=tibble(time=fitd$time,surv=fitd$surv,LL=fitd$lower,UL=fitd$upper)%>%slice(kd)
# Dd
# tail(Dd)
# (svd=fitd$surv[kd])
gy=ylab("Relative Survival")
gh=geom_hline(yintercept=0.6)
agy=scale_y_continuous(breaks=seq(0,1,.2),limits=c(0,1))#age times
gtc=function(sz) theme_classic(base_size=sz);
pRS=tibble(x,svr=Dd$surv/Dc$surv)
pRS%>%ggplot(aes(x,svr))+geom_step()+gx+gy+gh+agy+gtc(14)  
# tibble(x,svr=svd/svc)%>%ggplot(aes(x,svr))+geom_step()+gx+gy+gh+agy+gtc(14)  
ggsave("uveal/outs/survRel.pdf",width=4.5,height=3) # Fig. 5
save(pRS,file="uveal/data/pRS.RData")
# tibble(x,svr=svd/svc)%>%ggplot(aes(x,svr))+geom_step()+gx+gy+
#   geom_hline(yintercept=0.6)+ypts
# ggsave("uveal/outs/survRel30.pdf",width=4.5,height=3)

######### make RS table 4 below
# Dd
# tbOSk
# tbMFSk
# tbOSk$time=x0
# tbMFSk$time=x0
x0=seq(5,35,5)
(kc5=match.closest(x0,Dc$time))
Dc5=Dc%>%slice(kc5)
# Dc5=tibble(time=fitc$time,svc=fitc$surv,LLc=fitc$lower,ULc=fitc$upper)%>%slice(kc5)
Dc5

(kd5=match.closest(x0,Dd$time))
Dd5=Dd%>%slice(kd5)
Dd5$time=x0
(dRS=Dd5%>%mutate_at(vars(surv:UL),function(x) x/Dc5$surv))
dRS=dRS%>%mutate(CI=mkCI(surv,LL,UL))
L=list(RStable4=dRS%>%select(Time=time,              #table 4
                      "Relalite Survival"=CI))
WriteXLS(L,ExcelFileName="uveal/outs/RelSurvTable.xlsx",AdjWidth=T) 
# 
# ######## use survminer tools?
# fit.null <- surv_fit(Surv(time, status) ~ 1, data = colon)
# fit1 <- surv_fit(Surv(time, status) ~ sex, data = colon)
# fit2 <- surv_fit(Surv(time, status) ~ adhere, data = colon)
# fit.list <- list(sex = fit1, adhere = fit2)
# surv_median(fit.null)
# surv_median(fit2)
# surv_median(fit.list)
# surv_median(fit.list, combine = TRUE)
# fit.list2 <- surv_fit(Surv(time, status) ~ sex, data = colon,
#                       group.by = "rx")
# surv_median(fit.list2)
# 
# ## p values for between curves
# fit.null <- surv_fit(Surv(time, status) ~ 1, data = colon)
# fit1 <- surv_fit(Surv(time, status) ~ sex, data = colon)
# fit2 <- surv_fit(Surv(time, status) ~ adhere, data = colon)
# fit.list <- list(sex = fit1, adhere = fit2)
# surv_pvalue(fit.null)
# surv_pvalue(fit2, colon)
# surv_pvalue(fit.list)
# surv_pvalue(fit.list, combine = TRUE)
# fit.list2 <- surv_fit(Surv(time, status) ~ sex, data = colon,
#                       group.by = "rx")
# surv_pvalue(fit.list2)
# surv_pvalue(fit.list2, combine = TRUE, get_coord = TRUE)
# 
# #####3 useless tables below KM plots
# fit<- survfit(Surv(time, status) ~ sex, data = lung)
# tables <- ggsurvtable(fit, data = lung, color = "strata", y.text = FALSE)
# tables$risk.table
# tables$cumevents
# tables$cumcensor
# 
# 
