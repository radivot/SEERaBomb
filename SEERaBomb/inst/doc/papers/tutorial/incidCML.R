rm(list=ls())  
library(tidyverse)
load("~/data/SEER/mrgd/cancDef.RData") 
d=canc%>%filter(cancer=="CML")%>%mutate(age=agedx+0.5,year=yrdx)
(d=d%>%count(race,sex,age,year))
load("~/data/SEER/mrgd/popsae.RData") # loads in popsae (extended to ages 85-99)
(p=popsae%>%count(race,sex,age,year,wt=py)%>%rename(py=n))
d=left_join(p,d)
d[is.na(d$n),"n"]=0 # missings values should be zeros
d=d%>%filter(age>=20,age<=80,year>=2000)
d=d%>%mutate(ageg=cut(age,seq(20,80,5)))
d=d%>%mutate(yrg=cut(year,c(2000,2005,2010,2015),dig.lab = 4,include.lowest=T))
d=d%>%group_by(race,sex,ageg,yrg)%>%
  summarize(age=mean(age),py=sum(py)/1e5,n=sum(n))%>%mutate(incid=n/py)
graphics.off() 
quartz(height=5,width=6)
theme_update(legend.position = c(.20, .73), 
             axis.text=element_text(size=rel(1.3)),
             axis.title=element_text(size=rel(1.5)),
             strip.text = element_text(size = rel(1.3)),
             legend.title = element_blank(),
             legend.key.size = unit(1, 'lines'),
             legend.text=element_text(size=rel(1.1))             )
ggplot(d,aes(x=age,y=incid,col=sex,shape=sex))+geom_point(size=3.3)+ 
  labs(x="Age (years)",
       y=expression(paste("CML Cases per ",10^5," Person Years"))) +  
  facet_grid(yrg~race) +
  scale_y_log10(breaks=c(1,2,5))+
  scale_x_continuous(breaks=c(25,50,75))
ggsave("~/Results/tutorial/incidCML.pdf") # Fig 3

