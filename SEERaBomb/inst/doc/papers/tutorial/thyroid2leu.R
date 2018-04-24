rm(list=ls()); library(tidyverse); library(SEERaBomb)
load("~/data/SEER/mrgd/popsae.RData") #get popsae
(p=popsae%>%count(race,sex,age,year,wt=py)%>%rename(py=n))
load("~/data/SEER/mrgd/cancDef.RData") #get canc 
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("CML","AML","ALL") #second cancers of interest
(d=canc%>%filter(cancer%in%c("thyroid",secs)))
pf=seerSet(d,p,Sex="Female") #pooled (races) females 
pm=seerSet(d,p,Sex="Male") 
pf=mk2D(pf,secondS=secs)# adds secs background rates to pf
pm=mk2D(pm,secondS=secs)
trts=c("rad.noChemo", "noRad.noChemo")
pf=csd(pf, brkst=c(0,1,2,3,5,10), brksa=c(0,60), trts=trts, firstS="thyroid")
pm=csd(pm, brkst=c(0,1,2,3,5,10), brksa=c(0,60), trts=trts, firstS="thyroid")
DF=bind_rows(pf$DF,pm$DF)
DF=DF%>%mutate(cancer2=fct_relevel(cancer2,"AML")) #make AML 1st factor level
DF$t=DF$t+c(0,0.075,0.15) #shift times to see overlaid error bars in plots
D=DF%>%group_by(int,rad,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
Dtop=D%>%mutate(grp=str_c(rad,": All Ages"))
D=DF%>%filter(rad=="Rad")
D=D%>%group_by(int,ageG,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D$ageG=c("Age 0-60","Age >60")[D$ageG]
Dbot=D%>%mutate(grp=str_c("Rad: ",ageG))
D=bind_rows(Dtop,Dbot)
D$grp=as_factor(D$grp)
graphics.off();quartz(width=4,height=2.5)
myt=theme(legend.position=c(.25, .95),legend.title=element_blank(),
          legend.direction="horizontal",legend.margin=margin(0,0,0,0),
          legend.key.height = unit(.25, 'lines'))
g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line()+myt+
  labs(x="Years Since Thyroid Cancer Diagnosis",y="Relative Risk of Leukemia")
g=g+facet_wrap(~grp)+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU),width=0.1)+coord_cartesian(ylim=c(0,15))
ggsave("~/Results/tutorial/thyroid2leu.pdf") # Fig 4B

# 
# 
# graphics.off();quartz(width=4,height=2.5)
# myt=theme(legend.position=c(.25, .95),legend.title=element_blank(),
#           legend.direction="horizontal",legend.margin=margin(0,0,0,0),
#           legend.key.height = unit(.25, 'lines'))
# g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line(size=1)+myt+
#   labs(x="Years Since Thyroid Cancer Diagnosis",y="Relative Risk of Leukemia")
# g=g+facet_grid(~rad)+geom_abline(intercept=1, slope=0) 
# g +geom_errorbar(aes(ymin=rrL,ymax=rrU)) +coord_cartesian(ylim = c(0, 10))
# ggsave("~/Results/tutorial/thyroid2leu.pdf") # Fig 5
# 
# g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line(size=1)+myt+
#   labs(x="Years Since Thyroid Cancer Diagnosis",y="Relative Risk of Leukemia")
# g=g+facet_grid(~ageG)+geom_abline(intercept=1, slope=0) 
# g +geom_errorbar(aes(ymin=rrL,ymax=rrU)) +coord_cartesian(ylim = c(0, 15))
# ggsave("~/Results/tutorial/thyroid2leuAge.pdf") # Fig 5B
