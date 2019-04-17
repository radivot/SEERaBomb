### thyr2CMLfig1d.R   
# source("CML/naga/common.R")#run it first directly
secs=c("CML")#second cancers of interest
(d=canc%>%filter(cancer%in%c("thyroid",secs)))
load("~/data/SEER/mrgd/popsae.RData")
pf=seerSet(d,popsae,Sex="Female");pm=seerSet(d,popsae,Sex="Male") 
pf=mk2D(pf,secondS=secs);pm=mk2D(pm,secondS=secs)
trts=c("rad.noChemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,6,10),brksa=c(0,60),trts=trts,firstS="thyroid")
pm=csd(pm,brkst=c(0,1,6,10),brksa=c(0,60),trts=trts,firstS="thyroid")
DF=bind_rows(pf$DF,pm$DF)
gx=xlab("Years Since Thyroid Cancer Diagnosis")
gy=ylab("Relative Risk of CML")
Ds=DF%>%group_by(sex,int,rad)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
Ds=Ds%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
Ds=Ds%>%filter(rad=="Rad")
summary(m1<-mle2(O~dpois(lambda=(1+k1^3*t^2*exp(c1-k1*t))*E),
                 # parameters=list(c1~sex,k1~sex),
                 # parameters=list(k1~sex),
                 # parameters=list(c1~sex),
                 method="Nelder-Mead",start=list(c1=2.8,k1=.22),data=Ds,
                 control=list(maxit=10000))) 
pd=data.frame(t=seq(0,15,0.1),E=100)
(ds=data.frame(sex=c("Male","Female")))
pd=merge(ds,pd)
pd$Err=predict(m1,pd)/pd$E
Ds%>%ggplot(aes(x=t,y=RR,shape=sex))+
  geom_point(size=3)+
  gx+gy+gh+tc(14)+jco+sbb+ltb+gp+gl+geRR+cc+
  geom_line(aes(y=Err),size=.5,alpha=.4,data=pd)+
  theme(legend.position=c(.62,.75),legend.title=element_blank())
ggsave("~/Results/CML/thyr2CMLfig2B.pdf",width=4,height=3)
#  END Figure 1. Plot of thyroid cancer to CML kinetics 
#  Begin Calculation of latency of fitted curve
(cf=coef(summary(m1)))
(m1=round(cbind(point=cf[,1],LL=cf[,1]-1.96*cf[,2],UL=cf[,1]+1.96*cf[,2]),2))
(P=signif(cf[,4],2))
parms=row.names(cf)
(str=str_c(parms," = ",m1[,1]," (",m1[,2],", ",m1[,3],"), P = ",P,collapse="; "))
del=0.1
t=seq(0,100,del)
k=0.49
p=k^3*t^2*exp(-k*t)/2
sum(p)*del  #prove we have a prob density
p=k^3*t^3*exp(-k*t)/2
sum(p)*del  # mean of the density with k at its mean is 6.1 y
k=0.36
p=k^3*t^3*exp(-k*t)/2
sum(p)*del  # mean with k at its lower limit => latency upper limit
k=0.62
p=k^3*t^3*exp(-k*t)/2
sum(p)*del  # mean with k at its upper limit => latency lower limit
# END calculations of latency of smooth curve in Figure 1

