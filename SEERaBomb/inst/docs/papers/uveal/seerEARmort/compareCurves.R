source("uveal/common/setup.R")
library(cmprsk)
library(tidyverse)
quantile(d$surv)
(d=d%>%select(yrdx,agedx,sex,surv,status,CODS))
(D1=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),0,status)))
fit=survfit(Surv(surv,status)~1,data=D1)
dkm=tibble(t=fit$time,y=1-fit$surv,Method="1-KM")
# dkm%>%ggplot(aes(t,y))+geom_step()
(D2=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),2,status)))
fitc=with(D2,cuminc(surv,status))
dci=tibble(t=fitc[[1]]$time,y=fitc[[1]]$est,Method="CI")
# dci%>%ggplot(aes(t,y))+geom_step()

(Ds=msd(d5,mrt,brkst<-c(0,0.5,1,2,3,4,5,6,8,10,12,15,20,25,30,35)))
Ds=Ds%>%rename(Group="sex")%>%select(Group,int,everything()) #%>%arrange(Group,t)
D1=foldD(Ds)%>%mutate(Group="Pooled")%>%select(int,Group,everything())

(levs=levels(D1$int))
paste0(levs,collapse=", ")
(tb=as_tibble(t(sapply(levs,function(x) getBinInfo(x[1],x)))))
(tb=tb%>%mutate(delta=UL-LL))
range(d$surv)
tb$delta[16]=7
tb$ear=D1$EAR
tb$t=D1$t
(tb=tb%>%mutate(prod=delta*ear,cprod=cumsum(prod),
                surv=exp(-cprod),y=1-surv,
                Method="1-exp(-sum(EAR))"))

t=seq(0,40,.01)
mylin=function(t,m1=0.0011,t1=14.4,t2=26.8) {
  y=ifelse(t<t1,m1*t,0)
  y=ifelse((t>=t1)&(t<t2),m1*t1-(m1*t1/(t2-t1))*(t-t1),y)
  y=ifelse(t>=t2,0,y)
  y
}
mygam=function(t,c=-0.37849645,k=0.39624240) k^2*(t/2)*exp(c-k*t)
pd=tibble(t,Lines=mylin(t),Gamma=mygam(t),Sum=Lines+Gamma)
pd=pd%>%gather(key="func",value="y",-t)

pS=pd%>%filter(func=="Sum")
pS=pS%>%mutate(cy=cumsum(0.01*y),y=1-exp(-cy),Method="1-exp(-int(h))")

library(bbmle)#O=EAR*PY+E
(s=summary(mod<-mle2(O~dpois(lambda=(mygam(t,c,k)+mylin(t,m1,t1,t2))*PY+E),  
                     method="Nelder-Mead",
                     start=list(c=0,k=1,m1=0.002,t1=10,t2=20),data=D1,
                     control=list(maxit=10000))))
library(broom)
mkCIm=function(est,sd) paste0(sprintf('%.4f',est)," (",
         sprintf('%.4f',est-1.96*sd),", ",
         sprintf('%.4f',est+1.96*sd),")") 
(params=tidy(mod)%>%mutate(CI=mkCIm(estimate,std.error)))

library(WriteXLS)
WriteXLS(list(Table5=params%>%select(Parameter=term,    #table 5
                                     "Estimate (95% CI)"=CI,
                                     "P Value"=p.value)),
         ExcelFileName="uveal/outs/TableModParams.xlsx",AdjWidth=T) 
# save(mod,file="uveal/data/earFit.RData")

D1%>%ggplot(aes(x=t,y=EAR))+gp+gx +
  geom_line(aes(x=t,y=y,col=func),data=pd)+
  tc(14)+ltb+ltp
ggsave("uveal/outs/poisRegEAR.pdf",width=4,height=3) #Figure 6

load("uveal/data/pRS.RData")
drs=pRS%>%mutate(t=x,y=1-svr,Method="1-RS")
D=bind_rows(list(dkm,dci,drs,tb%>%select(t,y,Method),pS%>%select(t,y,Method)))
D%>%ggplot(aes(t,y,col=Method))+geom_step()+gx+
  ylab(" Probability of Death by UVM")+tc(14)+ltb+
  theme(legend.position=c(.6,.28)) +
  scale_colour_manual(name = "Method", 
   values = c("red","orange","green","blue", "violet"), 
   labels = expression(exp(-integral(hds)),exp(-sum(EAR)),1-MFS,1-RS,CompetingRisk))  
ggsave("uveal/outs/compareFig7.pdf",width=4.5,height=3.5)
