# france2019.R
graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci);library(bbmle)
load("~/data/SEER/mrgd/cancDef.RData")#load SEER cancer data
load("~/data/usMort/mrt.RData")#loads US mortality data
gp=geom_point();gl=geom_line()
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
gh=geom_hline(yintercept=1)
gh2=geom_hline(yintercept=2,col="gray")
svts=scale_x_continuous(breaks=c(0,5,10))#surv times
agts=scale_x_continuous(breaks=c(25,50,75))#age times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
sy=scale_y_log10()
cc=coord_cartesian(ylim=c(0,10))#clips high errorbars
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
gxi=xlab("Age (Years)")
gyi=ylab(quote(paste("Cases per ",10^5," Person Years")))

d=canc%>%filter(cancer=="CML")%>%print(n=13)
d%>%summarize(n=n(),na=sum(is.na(surv)),prct=100*na/n)#<2% missing
d=d%>%filter(!is.na(surv))
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)%>%print(n=13)
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1975,2001,2017)))
D=D%>%filter(Years!=levels(D$Years)[1]) 
D=D%>%group_by(int,Years)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
gx=xlab("Years Since CML Diagnosis")
gy=ylab("Relative Risk of Mortality")
myt=theme(legend.text=element_text(size=12),strip.text=element_text(size=12))
D%>%ggplot(aes(x=t,y=RR))+gp+gx+gy+gh+gh2+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR+myt+scale_y_continuous(breaks=c(1,2,5,10,15))

summary(m1<-mle2(O~dpois(lambda=(1+k1*exp(c1-k1*t)+k2*exp(c2-k2*t)+c3)*E),
                 # parameters=list(c3~sex),
                 method="Nelder-Mead",start=list(c1=0,c2=0,k1=1,k2=1,c3=1),data=D,
                 control=list(maxit=10000)))  #-2 log L: 187.3446 
pd=data.frame(t=seq(0,15,0.1),E=100)
(ds=data.frame(sex=c("Male","Female")))
pd=merge(ds,pd)
pd$Err=predict(m1,pd)/pd$E
head(pd)

D%>%ggplot(aes(x=t,y=RR))+gp+gx+gy+gh+gh2+ #facet_wrap(~sex)+
  geom_line(aes(y=Err),size=.5,alpha=.4,data=pd)+coord_cartesian(ylim=c(0,10))+
  svts+jco+tc(14)+ltb+ltp+sbb+ylim(c(0,10))+geRR+myt+scale_y_continuous(breaks=c(1,2,5,10))
ggsave("~/Results/CML/france19A.pdf",width=4,height=3)

sm1=summary(m1)
(cf=coef(sm1))
(m1=round(cbind(point=cf[,1],LL=cf[,1]-1.96*cf[,2],UL=cf[,1]+1.96*cf[,2]),2))
(P=signif(cf[,4],2))
parms=row.names(cf)
(str=str_c(parms," = ",m1[,1]," (",m1[,2],", ",m1[,3],"), P = ",P,collapse="; "))


library(survminer);library(survival)
ds=d%>%filter(yrdx>2001)
Ds=simSurv(ds,mrt)
fit=survfit(Surv(surv,status)~type,data=Ds)
gy=ylab("Survival Probability")
labs=c("Observed","Expected")
ggsurvplot(fit,Ds,legend.title="",legend.labs=labs,break.time.by=5,xlim=c(0,12),xlab="Years Since Diagnosis") 
ggsave("~/Results/CML/france19B.pdf",width=4,height=3)
Ds
S=summary(fit)
I=which((S$time>9.999)&(S$time<10.001))
S$time[I]
S$surv[I]
S$surv[I]^2/S$surv[I]
S$upper[I]
S$lower[I]
head(S)


