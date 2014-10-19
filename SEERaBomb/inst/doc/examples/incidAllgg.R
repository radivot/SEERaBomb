# using dplyr with SEER data
rm(list=ls()) 
library(dplyr)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsa.RData")


p=popsa%.%
  group_by(sex,age86)%.%
  summarise(py=sum(py)/1e5)
p
head(canc)
d=canc%.%
  select(cancer,sex,age86)%.%
  group_by(cancer,sex,age86) %.%
  summarise(cases=n())
d

sum(d$cases) #8.2M cases
sum(p$py)/1e4 # 1.7B PY

(D=left_join(d,p))
d=D%.%mutate(incidence=cases/py)

graphics.off()
theme_set(theme_gray(base_size = 24)) 
# theme_update(legend.position = "top")
if(length(grep("linux",R.Version()$os))) windows <- function( ... ) X11( ... )
if(length(grep("darwin",R.Version()$os))) windows <- function( ... ) quartz( ... )
windows(width=12,height=7)
ggplot(aes(x=age86,y=incidence,col=sex),data=d)+ #log="y",
      ylab("Incidence (Cases/100,000 Person-Years)")+
      xlab("Age")+geom_line(size=1)+facet_wrap(~cancer) +
  scale_y_log10(breaks=c(.01,1,100),labels=c(".01","1","100")) + theme( legend.position = c(.87, .075))  
ggsave("~/ccf/tomR/incidMat.eps")

d%.%filter(cancer%in%c("breast","lung","prostate"))%.%summarise(max(incidence))


#now compare male and female incidences summed over all cancers
windows(width=7,height=7)
d=canc%.%
  select(sex,age86)%.%
  group_by(sex,age86,add=F) %.%
  summarise(cases=n())
(D=left_join(d,p))
d=D%.%mutate(incidence=cases/py)
ggplot(aes(x=age86,y=incidence,col=sex),data=d)+ #log="y",
  ylab("Incidence (Cases/100,000 Person-Years)")+
  xlab("Age")+geom_line(size=1)+geom_vline(xintercept = c(40,50,65),col="dark gray")+
  scale_y_log10(breaks=c(.01,1,100,1000),labels=c(".01","1","100","1000")) + theme( legend.position = c(.87, .075))  
 ggsave("~/ccf/tomR/sumMvsF.eps")


# now zoom in on screened cancers and note discontinuities at screening ages of 40, 50 and 65
windows(width=12,height=7)
d=canc%.%
  filter(cancer%in%c("colon","rectal","prostate","breast","CIS"))%.%
  select(cancer,sex,age86)%.%
  group_by(cancer,sex,age86) %.%
  summarise(cases=n())
(D=left_join(d,p))
d=D%.%mutate(incidence=cases/py)
qplot(age86,incidence,col=sex,data=d,xlim=c(32,73),#log="y", #ylim=c(1,1e3),
      ylab="Incidence (Cases/100,000 Person-Years)",
      xlab="Age")+geom_line()+geom_vline(xintercept = c(40,50,65),col="dark gray")+
  facet_wrap(~cancer)  +theme(strip.text = element_text(size = rel(2))) +
  scale_y_log10(breaks=c(10,100,1000),labels=c("10","100","1000"),limits=c(10,1000))
ggsave("~/ccf/tomR/zoomScreenYrs.eps")



