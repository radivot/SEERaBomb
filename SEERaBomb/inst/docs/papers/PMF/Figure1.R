##### intermediate granularity mesh
source("pmf/setup.R")  
library(mgcv)
load("pmf/data/maxRes.RData")  # run mkMaxRes.R to create this
(D=m%>%select(s=sex,a,t,O:PY)%>%arrange(s,a,t))
Dm=D%>%filter(s=="Male")
Df=D%>%filter(s=="Female")
n=8
foldBins=function(m) {
  m=m%>%mutate(tg=cut(t,breaks=quantile(t,seq(0,1,1/n)),include.lowest = T))
  m=m%>%mutate(ag=cut(a,breaks=quantile(a,seq(0,1,1/n)),include.lowest = T))
  m%>%group_by(tg,ag)%>%summarize(t=mean(t),a=mean(a),O=sum(O),E=sum(E),PY=sum(PY),
                                  incid=log10(O/PY),back=log10(E/PY))
}
(Dm=foldBins(Dm))
(Df=foldBins(Df))

bm0 <- gam(O ~ a  + offset(log(PY)),family = poisson(),data=Dm)
bm1 <- gam(O ~ a + t + offset(log(PY)),family = poisson(),data=Dm)
bmast <- gam(O ~ a + s(t) + offset(log(PY)),family = poisson(),data=Dm)
bm2 <- gam(O ~ s(a) + s(t) + offset(log(PY)),family = poisson(),data=Dm) 
bm3 <- gam(O ~ s(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
bm4 <- gam(O ~ ti(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
bm5 <- gam(O ~ s(a) + s(t) +ti(a,t) + offset(log(PY)),family = poisson(),data=Dm) 
AIC(bm0,bm1,bmast,bm2,bm3,bm4,bm5)%>%rownames_to_column%>%mutate(dAIC=AIC-min(AIC))  
BIC(bm0,bm1,bmast,bm2,bm3,bm4,bm5)%>%rownames_to_column%>%mutate(dBIC=BIC-min(BIC))  
bm=bm5

bfa <- gam(O ~ a  + offset(log(PY)),family = poisson(),data=Df)
bfsa <- gam(O ~ s(a)  + offset(log(PY)),family = poisson(),data=Df) 
bft <- gam(O ~ t  + offset(log(PY)),family = poisson(),data=Df)
bfat <- gam(O ~ a + t + offset(log(PY)),family = poisson(),data=Df)
bfast <- gam(O ~ a + s(t) + offset(log(PY)),family = poisson(),data=Df)
bf4 <- gam(O ~ s(a) + s(t) + offset(log(PY)),family = poisson(),data=Df) 
bf5 <- gam(O ~ s(a) + s(t) +ti(a,t)+ offset(log(PY)),family = poisson(),data=Df) 
AIC(bfa,bfsa,bft,bfat,bfast,bf4,bf5)%>%rownames_to_column%>%mutate(dAIC=AIC-min(AIC))    
BIC(bfa,bfsa,bft,bfat,bfast,bf4,bf5)%>%rownames_to_column%>%mutate(dBIC=BIC-min(BIC))    
bf=bfa # use BIC to avoid over fitting

times=seq(0,10,0.5) #go to 11 for females to avoid axis label overlap
ages=seq(40,90,5)
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
with(Dm,plot3d(a,t,incid,xlab="",ylab="",zlab="",col="violet",radius=0.5,type="s")) 
with(dpm,surface3d(x=ages,y=times,z=matrix(log10(e/PY),ncol=nt,byrow=TRUE),alpha=0.6,col="red")) 
with(Dm,surface3d(x=a[1:n],y=t[seq(1,n*n,n)],z=matrix(log10(E/PY),ncol=n,byrow=F),alpha=0.8,col="yellow")) 

clear3d(type="lights")
light3d(theta = -90, phi = 75) 
with(Df,plot3d(a,t,incid,xlab="",ylab="",zlab="",col="violet",radius=.5,type="s")) 
with(dpf,surface3d(x=ages,y=times,z=matrix(log10(e/PY),ncol=nt,byrow=TRUE),alpha=0.6,col="red")) 
with(Df,surface3d(x=a[1:n],y=t[seq(1,n*n,n)],z=matrix(log10(E/PY),ncol=n,byrow=F),alpha=0.8,col="yellow")) 
save(bm,bf,file="pmf/data/models.RDA")
save(Dm,dpm,Df,dpf,file="pmf/data/Ddp.RDA")

