rm(list=ls()) 
library(SEERaBomb)
library(ggplot2)
library(dplyr)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popga.RData") 

(tb=sort(table(canc$cancer),dec=T))
# CANCS=names(tb)[1:9]  
CANCS=c("AML","CML","MDS")
CANCS=names(tb)  
CANCS=c("stomach","lung","melanoma","prostate")
D=canc
mds="all"
mds="clean"
cutyr=2003
if (mds=="clean") D=canc%.%filter(yrdx>=cutyr,cancer%in%CANCS,histo3!=9989) else
  D=canc%.%filter(yrdx>=cutyr,cancer%in%CANCS)



D=D%.%mutate(year=yrdx) #let year=yrdx to match population column below
P=popga%.%filter(year>=cutyr)
r=data.frame(reg=levels(D$reg))
c=data.frame(cancer=CANCS)
cr=merge(c,r)
head(D)
m=D%.%group_by(cancer,db,reg,age19)%.%summarize(cases=n())

p=P%.%group_by(db,reg,age19)%.%summarise(py=sum(py))
p=left_join(cr,p)
d=left_join(p,m)
d[is.na(d$cases),"cases"]=0 #join left missings where zero's should be, so fix this

head(d)

d1=d%.%  
  group_by(cancer,db,reg,age19,add=F)%.%
  summarize(cases=sum(cases),py=sum(py)/1e5)%.%
  left_join(us19)%.%
  mutate(ai=cases/py)%.%
  summarize(vr=sum(cases*prop^2/py^2),stdIncid=weighted.mean(ai,w=prop),
            stdLo=stdIncid-1.96*sqrt(vr),stdHi=stdIncid+1.96*sqrt(vr))

# head(d1)
graphics.off()
if(length(grep("linux",R.Version()$os))) windows <- function( ... ) X11( ... )
if(length(grep("darwin",R.Version()$os))) windows <- function( ... ) quartz( ... )
windows(width=12,height=7)


theme_set(theme_gray(base_size = 12))
# theme_get()
theme_update(
#   axis.title = element_text(size = rel(3)),
  legend.direction = "horizontal" #, # c(.96, .56),  
#       plot.margin = unit(c(.5,1,.5,.5),"lines")
)
# theme_get()
  


ggplot(aes(x=reg,y=stdIncid,col=db),data=d1)+ 
  ylab("Age-adjusted Incidence (Cases/100,000 PY) US2000 Std")+
  xlab("SEER Registry") + 
  ggtitle(paste0("Cancers in ",cutyr,"-2011")) + 
  geom_errorbar(aes(ymin=stdLo,ymax=stdHi))+
  scale_colour_hue("Dataset", breaks = c("73", "92", "00"), labels = c("1973", "1992", "2000"))+
  theme(axis.title = element_text(size = rel(1.2)),
        legend.position = c(.87, .075),  
        axis.text.y = element_text(size = rel(1.5)),
        plot.title = element_text(size = rel(2)),
        axis.title.x = element_text(size = rel(1.7)),
        axis.title.y = element_text(size = rel(1.3)),
        strip.text = element_text(size = rel(2))
        ) +
  facet_wrap(~cancer)

# if (length(CANCS)>3) ggsave("~/ccf/tomR/cancersVsregs.eps") else
#     ggsave(paste0("~/ccf/tomR/mlVsregsMDS",mds,".eps"))


