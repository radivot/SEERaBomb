graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse)
library(mgcv)
library(rgl)
load("pmf/data/maxRes.RData")
m
(D=m%>%select(s=sex,a,t,O:PY)%>%arrange(s,a,t))
D$incid=log10(D$O/D$PY)
min(D$incid)
D$back=log10(D$E/D$PY)
min(D$back)
D
Dm=D%>%filter(s=="Male")
Df=D%>%filter(s=="Female")

bm0 <- gam(O ~ a  + offset(log(PY)),family = poisson(),data=Dm)
bm1 <- gam(O ~ a + t + offset(log(PY)),family = poisson(),data=Dm)
bmast <- gam(O ~ a + s(t) + offset(log(PY)),family = poisson(),data=Dm)
bm2 <- gam(O ~ s(a) + s(t) + offset(log(PY)),family = poisson(),data=Dm) 
bm3 <- gam(O ~ s(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
bm4 <- gam(O ~ ti(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
bm5 <- gam(O ~ s(a) + s(t) +ti(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
AIC(bm0,bm1,bmast,bm2,bm3,bm4,bm5)%>%rownames_to_column%>%mutate(dAIC=AIC-min(AIC)) 
BIC(bm0,bm1,bmast,bm2,bm3,bm4,bm5)%>%rownames_to_column%>%mutate(dBIC=BIC-min(BIC))  
bm=bm0

bfa <- gam(O ~ a  + offset(log(PY)),family = poisson(),data=Df)
bfsa <- gam(O ~ s(a)  + offset(log(PY)),family = poisson(),data=Df) 
bft <- gam(O ~ t  + offset(log(PY)),family = poisson(),data=Df)
bfat <- gam(O ~ a + t + offset(log(PY)),family = poisson(),data=Df)
bfast <- gam(O ~ a + s(t) + offset(log(PY)),family = poisson(),data=Df)
bf4 <- gam(O ~ s(a) + s(t) + offset(log(PY)),family = poisson(),data=Df) 
bf5 <- gam(O ~ s(a) + s(t) +ti(a,t)+ offset(log(PY)),family = poisson(),data=Df) 
AIC(bfa,bfsa,bft,bfat,bfast,bf4,bf5)%>%rownames_to_column%>%mutate(dAIC=AIC-min(AIC))  
BIC(bfa,bfsa,bft,bfat,bfast,bf4,bf5)%>%rownames_to_column%>%mutate(dBIC=BIC-min(BIC))    
bf=bf5
bf=bfa


times=seq(0,13,0.5)
ages=seq(20,100,5)
(nt=length(times))
(na=length(ages))
s=c("Female","Male")
(dp=expand_grid(s,a=ages, t=times,PY=100))
dpm=dp%>%filter(s=="Male")
dpf=dp%>%filter(s=="Female")

Dm$e=exp(predict(bm,newdata=Dm)) 
dpm$e=exp(predict(bm,newdata=dpm)) 
Df$e=exp(predict(bf,newdata=Df)) 
dpf$e=exp(predict(bf,newdata=dpf)) 

clear3d(type="lights")
light3d(theta = -90, phi = 75) 
with(Dm,plot3d(a,t,incid,xlab="",ylab="",zlab="",col="violet",radius=.2,type="s")) 
with(dpm,surface3d(x=ages,y=times,z=matrix(log10(e/PY),ncol=nt,byrow=TRUE),alpha=0.8,col="red")) 

clear3d(type="lights")
light3d(theta = -90, phi = 75) 
with(Df,plot3d(a,t,incid,xlab="",ylab="",zlab="",col="violet",radius=.2,type="s")) 
with(dpf,surface3d(x=ages,y=times,z=matrix(log10(e/PY),ncol=nt,byrow=TRUE),alpha=0.8,col="red")) 

