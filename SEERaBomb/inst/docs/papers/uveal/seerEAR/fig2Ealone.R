graphics.off();rm(list=ls())#clear plots and environment 
library(tidyverse)
load("data/D1.RData")

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
pS=pS%>%mutate(cy=cumsum(0.01*y),y=1-exp(-cy),Method="Excess Absolute Risk")

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
WriteXLS(list(Table5=params%>%select(Parameter=term,  
                                     "Estimate (95% CI)"=CI,
                                     "P Value"=p.value)),
         ExcelFileName="outs/TableModParams.xlsx",AdjWidth=T) 

pd=pd%>%mutate(comp=ifelse(func=="Lines","Triangle",func),func=as_factor(comp))

gp=geom_point() 
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
tc=function(sz) theme_classic(base_size=sz)
svts=scale_x_continuous(breaks=seq(0,35,5),limits=c(0,38))
gx=xlab("Years Since Diagnosis")
gy=ylab("Excess Absolute Risk of Mortality")
gh0=geom_hline(yintercept=0)

D1%>%ggplot(aes(x=t,y=EAR))+gp+gx + ge+ gy+
  geom_line(aes(x=t,y=y,col=func),data=pd)+
  tc(14)+ltb+ltp
ggsave("outs/EARfig2Ealone.pdf",width=4,height=3)
