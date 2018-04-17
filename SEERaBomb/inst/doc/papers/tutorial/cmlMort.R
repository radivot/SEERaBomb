library(SEERaBomb)
# to create these files see pdfs overview and gettingData in doc folder
load("~/data/usMort/mrt.RData") # loads mrt
load("~/data/SEER/mrgd/cancDef.RData") #loads canc
d=canc%>%filter(cancer=="CML")
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)
d%>%group_by(sex)%>%summarize(med=median(agedx))
brks=c(0,0.5,1,2,3,4,5,6,8) 
ybrks=c(1972,1990,2002,2013)
D=msd(d,mrt,brks,ybrks)
head(D,2)
quartz(width=6,height=5)
theme_update(legend.position = c(.88, .37),axis.text=element_text(size=rel(1.4)),
    axis.title=element_text(size=rel(1.4)),strip.text = element_text(size = rel(1.5)))
g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"), ylim=c(0,18),
        xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality")
g=g+geom_line(size=1)+facet_grid(sex~.)+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15)) 
ggsave("~/Results/CML/mortRRtimeCrsTrends.png")  
  
             