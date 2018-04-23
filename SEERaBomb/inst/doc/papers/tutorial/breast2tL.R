rm(list=ls()); library(tidyverse); library(SEERaBomb)
load("~/data/SEER/mrgd/popsae.RData") #get popsae
(p=popsae%>%count(race,sex,age,year,wt=py)%>%rename(py=n))
load("~/data/SEER/mrgd/cancDef.RData") #get canc 
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("CML","AML","ALL") #second cancers of interest
(d=canc%>%filter(sex=="Female",cancer%in%c("breast",secs)))
pf=seerSet(d,p,Sex="Female") #pooled (races) females 
pf=mk2D(pf,secondS=secs)# adds secs background rates to pf
trts=c("rad.chemo", "rad.noChemo", "noRad.chemo", "noRad.noChemo")
(pf=csd(pf, brkst=c(0,1,2,3,5,10), brksa=c(0,60), trts=trts, firstS="breast"))
(D=pf$DF%>%filter(ageG=="(0,60]")%>%select(int:rrU,-(n:sig2),-(yearG:E)))
D$trt=D$trt%>%str_replace_all("no","No ")
(D=D%>%separate(trt,c("rad","chemo"),sep="[\\.]",fixed=T))
(D=D%>%mutate_at(vars(rad:chemo),funs(str_to_title)))
D$cancer2=fct_relevel(D$cancer2,"AML") #make AML 1st factor level
D$t=D$t+c(0,0.075,0.15) #shift times to see overlaid error bars
graphics.off(); quartz(width=6,height=4)
theme_update(legend.position = c(.28,.32), legend.title=element_blank(),
             legend.text=element_text(size=rel(1.1)),
             axis.text=element_text(size=rel(1.3)),
             axis.title=element_text(size=rel(1.5)),
             strip.text = element_text(size = rel(1.3))) 
g=ggplot(aes(x=t,y=RR,col=cancer2),data=D)+geom_point()+geom_line(size=1)+
  labs(x="Years Since Breast Cancer Diagnosis",y="Relative Risk of Leukemia")
g=g+facet_grid(rad~chemo)+geom_abline(intercept=1, slope=0) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU)) 
ggsave("~/Results/tutorial/breast2leu.pdf") # Fig 4
D%>%filter(cancer2=="CML",rad=="Rad") # see values of CML RR CI at peaks
pf$DF%>%group_by(trt)%>%summarize(O=sum(O),E=sum(E),meanPYage=weighted.mean(age,w=py)) 
