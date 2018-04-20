library(SEERaBomb)
load("~/data/usMort/mrt.RData") # loads mrt
load("~/data/SEER/mrgd/cancDef.RData") #loads canc
d=canc%>%filter(cancer=="CML")
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1972,1990,2005,2015)))
quartz(width=4,height=3)
theme_update(legend.position = "top")
g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"), ylim=c(0,18),
        xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality")
g=g+geom_line(size=1)+facet_grid(sex~.)+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15)) 
ggsave("~/Results/tutorial/CMLmortRRtimeCrsTrends.pdf")  

