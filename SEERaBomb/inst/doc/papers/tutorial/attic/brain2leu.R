# results here were too noisy to include in the paper
rm(list=ls()); library(tidyverse); library(SEERaBomb)
load("~/data/SEER/mrgd/popsae.RData") #get popsae
(p=popsae%>%count(race,sex,age,year,wt=py)%>%rename(py=n))
load("~/data/SEER/mrgd/cancDef.RData") #get canc 
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("CML","AML","ALL") #second cancers of interest
(d=canc%>%filter(cancer%in%c("brain",secs)))
d%>%count(cancer,trt)%>%print(n=30)
pf=seerSet(d,p,Sex="Female") #pooled (races) females 
pm=seerSet(d,p,Sex="Male") 
pf=mk2D(pf,secondS=secs)# adds secs background rates to pf
pm=mk2D(pm,secondS=secs)
trts=c("rad.chemo", "rad.noChemo", "noRad.chemo", "noRad.noChemo")
pf=csd(pf, brkst=c(0,1,2,3,5,10), brksa=c(0,60), trts=trts, firstS="brain")
pm=csd(pm, brkst=c(0,1,2,3,5,10), brksa=c(0,60), trts=trts, firstS="brain")
DF=bind_rows(pf$DF,pm$DF)
DF=DF%>%mutate(cancer2=fct_relevel(cancer2,"AML")) #make AML 1st factor level
DF$t=DF$t+c(0,0.075,0.15) #shift times to see overlaid error bars in plots
D=DF%>%group_by(int,rad,chemo,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
graphics.off();quartz(width=4,height=2.5)
myt=theme(legend.position=c(.25, .95),legend.title=element_blank(),
          legend.direction="horizontal",legend.margin=margin(0,0,0,0),
          legend.key.height = unit(.25, 'lines'))
g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line()+myt+
  labs(x="Years Since Brain Cancer Diagnosis",y="Relative Risk of Leukemia")
g=g+facet_grid(rad~chemo)+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU),width=0.1) +coord_cartesian(ylim=c(0,30))
ggsave("~/Results/tutorial/brain2leu.pdf") 

