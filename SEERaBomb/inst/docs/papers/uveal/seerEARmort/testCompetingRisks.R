source("uveal/common/setup.R")
library(cmprsk)
library(bbmle)
N=100
dm=tibble(yrdx=1980,agedx=40,sex="Male",surv=rnorm(2*N,mean=3,sd=1),status=1,CODS="melanoma")
dm=tibble(yrdx=1980,agedx=40,sex="Male",surv=rnorm(2*N,mean=10,sd=3),status=1,CODS="melanoma")
do=tibble(yrdx=1980,agedx=40,sex="Male",surv=rnorm(N,mean=10,sd=3),status=1,CODS="other")
do=tibble(yrdx=1980,agedx=40,sex="Male",surv=rnorm(N,mean=3,sd=1),status=1,CODS="other")
dc=tibble(yrdx=1980,agedx=40,sex="Male",surv=rnorm(N,mean=20,sd=5),status=0,CODS="alive")
d=bind_rows(dm,do,dc)
(D1=d%>%mutate(status=ifelse((status==1)&(CODS!="melanoma"),0,status)))
# (D1=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),0,status)))
fit=survfit(Surv(surv,status)~1,data=D1)
dkm=tibble(t=fit$time,y=1-fit$surv,Method="1-KM")
dkm%>%ggplot(aes(t,y))+geom_step()
(D2=d%>%mutate(status=ifelse((status==1)&(CODS!="melanoma"),2,status)))
# (D2=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),2,status)))
fitc=with(D2,cuminc(surv,status))
dci=tibble(t=fitc[[1]]$time,y=fitc[[1]]$est,Method="CI")
dci%>%ggplot(aes(t,y))+geom_step()
(Ds=msd(d,mrt,brkst<-c(0,0.5,1,2,3,4,5,6,8,10,12,15,20,25,30,35)))
Ds=Ds%>%rename(Group="sex")%>%select(Group,int,everything()) #%>%arrange(Group,t)
D1=foldD(Ds)%>%mutate(Group="Pooled")%>%select(int,Group,everything())

(levs=levels(D1$int))
(tb=as_tibble(t(sapply(levs,function(x) getBinInfo(x[1],x)))))
(tb=tb%>%mutate(delta=UL-LL))
range(d$surv)
tb$delta[16]=7
tb$ear=D1$EAR
tb$t=D1$t
(tb=tb%>%mutate(prod=delta*ear,cprod=cumsum(prod),surv=exp(-cprod),y=1-surv,Method="1-exp(-sum(EAR))"))
D=bind_rows(list(dkm,dci,tb%>%select(t,y,Method)))
gtc=function(sz) theme_classic(base_size=sz);
D%>%ggplot(aes(t,y,col=Method))+geom_step()+gx+ylab(" Probability of Death by UVM")+gtc(14)+ltb+
  theme(legend.position=c(.6,.28))

mylin=function(t,m1,t1,t2) {
  y=ifelse(t<t1,m1*t,0)
  y=ifelse((t>=t1)&(t<t2),m1*t1-(m1*t1/(t2-t1))*(t-t1),y)
  y=ifelse(t>=t2,0,y)
  y
}
mygam=function(t,c,k) k^2*(t/2)*exp(c-k*t)
summary(mod<-mle2(O~dpois(lambda=(mygam(t,c,k)+mylin(t,m1,t1,t2))*PY+E),  #O=EAR*PY+E
                  method="Nelder-Mead",start=list(c=0,k=1,m1=0.02,t1=2,t2=10),data=D1,
                  control=list(maxit=10000))) 
D1


pd=data.frame(t=seq(0,42,0.1),E=sum(D1$E),PY=sum(D1$PY))
pd=data.frame(t=seq(0,42,0.1),E=1000,PY=10)
pd$y=predict(mod,pd)/pd$E
head(pd)

D1%>%ggplot(aes(x=t,y=EAR))+gp+gx +
  geom_line(aes(x=t,y=y),data=pd)+
  gtc(14)+ltb+ltp


ggsave("uveal/outs/poisRegEARfake.pdf",width=4,height=3)


(pS=pd%>%filter(func=="Sum")%>%mutate(cy=cumsum(0.01*y),y=1-exp(-cy),Method="1-exp(-sum(EARfit))"))

D=bind_rows(list(dkm,dci,tb%>%select(t,y,Method),pS%>%select(t,y,Method)))
D%>%ggplot(aes(t,y,col=Method))+geom_step()+gx+ylab(" Probability of Death by UVM")+gtc(14)+ltb+
  theme(legend.position=c(.6,.28))
ggsave("uveal/outs/fig5earFake.pdf",width=4.5,height=3.3)

# 
# mylinp=function(t,p=c(m1,t1,t2)) {
#   with(as.list(p),{
#     y=ifelse(t<t1,m1*t,0)
#     y=ifelse((t>=t1)&(t<t2),m1*t1-(m1*t1/(t2-t1))*(t-t1),y)
#     y=ifelse(t>=t2,0,y)
#     y})
# }
# mygamp=function(t,p=c(c,k)) with(as.list(p),k^2*(t/2)*exp(c-k*t))
# 
# summary(modp<-mle2(O~dpois(lambda=(mygamp(t,c(c,k))+mylinp(t,c(m1,t1,t2)))*PY+E),  #O=EAR*PY+E
#                    method="Nelder-Mead",start=list(c=0,k=1,m1=0.002,t1=10,t2=20),data=D1,
#                    control=list(maxit=10000))) 
# 
