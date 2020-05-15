source("uveal/common/setup.R")
# 2.  can we tell from SEER data that people in cured fraction were young and
# had smaller tumors?   #TR could be artifact of being young to live along enough 
# 4. Is old age poor prognosis (usually accepted to be so) is an
# artefact; Is there a difference between young and old (and other groups of
# SEER) that is statistcally significant ??
(d6=d%>%select(yrdx,agedx,sex,surv,status,CODS))
(d6=d6%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),0,status))%>%select(-CODS))
d6=d6%>%mutate(ageG=cut(agedx,c(0,20,40,50,60,70,80,90,100)))
table(d6$ageG)
fit6=survfit(Surv(surv,status)~ageG,data=d6) 
ggsurvplot(fit6,d6,legend.title="",legend.labs=levels(d6$ageG),legend=c(.2,.3))+gx 
ggsave("uveal/outs/kmMFSageG.pdf",width=4.5,height=4.5)

library(cmprsk)
(d=d%>%select(yrdx,agedx,sex,surv,status,CODS))
(D2=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),2,status)))
D2=D2%>%mutate(ageG=cut(agedx,c(0,20,40,50,60,70,80,90,100)))
fitc=with(D2,cuminc(surv,status,ageG))
labs=c("Death by Melanoma","Death by Other Causes")
plot(fitc,curvlab=labs,xlab="Years Since Diagnosis",ylab="Cumulative Probability of Death")
(g=ggcompetingrisks(fitc)+sbb+gx+ylab("Cumulative Probability of Death")+
    scale_color_manual(labels = labs, values = c("#F8766D", "#00BFC4")) +
    theme(legend.position = "top",legend.title=element_blank()) +labs(title="",caption=""))
ggsave("uveal/outs/cumIncAgeG.pdf",width=4.5,height=4.5)
str(fitc)
fitc[[1]]
L=fitc[1:16]
lapply(L,function(x) x[["est"]])
sapply(L,function(x) tail(x$est,1))[1:8]

##### OS plots give typical dependence
range(d5$agedx)
d5=d5%>%mutate(ageG=cut(agedx,c(0,20,40,50,60,70,80,90,100)))
table(d5$ageG)
fit5=survfit(Surv(surv,status)~ageG,data=d5) 
ggsurvplot(fit5,d5,legend.title="",legend.labs=levels(d5$ageG))+gx
ggsave("uveal/outs/kmOSageG.pdf",width=4.5,height=4.5)


ddT=simSurv(d5,mrt,rep=1,ltb=NULL,unif=TRUE)
ddT=ddT%>%mutate(ageG=cut(agedx,c(0,20,40,50,60,70,80,90,100)))%>%filter(type=="Simulated")
fit=survfit(Surv(surv,status)~ageG,data=ddTs) 
ggsurvplot(fit,ddT,legend.title="",legend.labs=levels(d6$ageG),xlim=c(0,43))+gx # +svts
ggsave("uveal/outs/kmOSsimulatedNormals.pdf",width=4.5,height=4.5)

