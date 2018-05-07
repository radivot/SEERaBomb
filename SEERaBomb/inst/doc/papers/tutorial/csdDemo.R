rm(list=ls());library(tidyverse);library(SEERaBomb)
load("~/data/SEER/mrgd/popsae.RData") 
(p=popsae%>%count(race,sex,age,year,wt=py)%>%rename(py=n))
load("~/data/SEER/mrgd/cancDef.RData") 
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("AML","ALL","CML") 
(d=canc%>%filter(sex=="Female",cancer%in%c("breast",secs)))
pf=seerSet(d,p,Sex="Female")#pooled (races) females 
pf=mk2D(pf,secondS=secs)#adds secs background rates to pf
trts=c("rad.chemo","rad.noChemo","noRad.chemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="breast")
(D=pf$DF%>%filter(ageG=="(0,60]")) 
myt=theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
          legend.direction="horizontal",legend.key.height=unit(.25,'lines'),
          legend.position=c(.25,.95))
ge=geom_errorbar(aes(ymin=rrL,ymax=rrU))
ylab="Relative Risk of Leukemia"
g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line()+
  labs(x="Years Since Breast Cancer Diagnosis",y=ylab)+myt+ge
g+facet_grid(rad~chemo)+geom_hline(yintercept=1)+coord_cartesian(ylim=c(0,25)) 
ggsave("~/Results/tutorial/breast2leu.pdf",width=4,height=2.5)#Fig.4A 


(d=canc%>%filter(cancer%in%c("thyroid",secs)))
pf=seerSet(d,p,Sex="Female");pm=seerSet(d,p,Sex="Male") 
pf=mk2D(pf,secondS=secs);pm=mk2D(pm,secondS=secs)
trts=c("rad.noChemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="thyroid")
pm=csd(pm,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="thyroid")
DF=bind_rows(pf$DF,pm$DF)
D=DF%>%group_by(int,rad,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
Dtop=D%>%mutate(grp=str_c(rad,": All Ages"))
D=DF%>%filter(rad=="Rad")
D=D%>%group_by(int,ageG,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D$ageG=c("Age 0-60","Age >60")[D$ageG]
Dbot=D%>%mutate(grp=str_c("Rad: ",ageG))
D=bind_rows(Dtop,Dbot)
D$grp=as_factor(D$grp)#orders by occurrence, as wanted
g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line()+
  labs(x="Years Since Thyroid Cancer Diagnosis",y=ylab)+myt+ge
g+facet_wrap(~grp)+geom_hline(yintercept=1)+coord_cartesian(ylim=c(0,15)) 
ggsave("~/Results/tutorial/thyroid2leu.pdf",width=4,height=2.5)


