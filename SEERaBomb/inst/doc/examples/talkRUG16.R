library(SEERaBomb)
####### make R binaries of SEER data ################### 
(df=getFields("/Users/radivot/data/SEER")) # same as getFields()
(rdf=pickFields(df)) # this uses default picks of fields
# mkSEER(rdf,seerHome="/Users/radivot/data/SEER") # same as mkSEER(rdf) 
rm(list=ls())
# mkSEER above makes the following two files
load("~/data/SEER/mrgd/cancDef.RData")#loads canc
load("~/data/SEER/mrgd/popsa.RData")
head(canc,2)
head(popsa,2)
(tb=sort(table(canc$cancer),dec=T)) #liver is top 5 world-wide, pancreas and ovary deadly, MDS since 2001

####### Cancer Incidences vs Age for all cancers ############ from SEERaBomb's doc/examples/incidAllgg.R
(p=popsa%>%group_by(sex,age86)%>%summarise(py=sum(py)/1e5))
(d=canc%>%select(cancer,sex,age86)%>%group_by(cancer,sex,age86)%>%summarise(cases=n()))
(D=left_join(d,p))
d=D%>%mutate(incidence=cases/py)



graphics.off()
theme_set(theme_gray(base_size = 16)) 
quartz(width=12,height=7)
ggplot(aes(x=age86,y=incidence,col=sex),data=d)+ 
  ylab("Incidence (Cases/100,000 Person-Years)")+
  xlab("Age")+geom_line(size=1)+facet_wrap(~cancer) +
  scale_y_log10(breaks=c(.01,1,100),labels=c(".01","1","100")) + theme( legend.position = c(.87, .05))  

ggplot(aes(x=age86,y=incidence,col=sex),data=d%>%filter(cancer%in%names(tb)[1:20]))+ 
  ylab("Incidence (Cases/100,000 Person-Years)")+
  xlab("Age")+geom_line(size=1)+facet_wrap(~cancer) +
  scale_y_log10(breaks=c(.01,1,100),labels=c(".01","1","100"))+theme(legend.position="top",strip.text=element_text(size=rel(1.4)))  

####### END background incidences vs age

######### Survival of MDS: Sex dependence #######
d=canc%>%filter(cancer=="MDS",yrdx>2001)
table(d$histo3)  #9920=tMN, 9960=>9975=MDS/MPN, 9984=REABT (obs), 9985=RCMD, 9986=del5q, 9987=tMDS (obs)
d=d%>%filter(histo3%in%c(9980,9982))# 9980=RA, 9982=RARS <= focus on these two   9983=RAEB, 9989 MDS NOS
d=d%>%mutate(cancer=factor(ifelse(histo3==9982,"RARS","RA")))
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)%>%filter(surv<200) #remove survival unknowns =9999 months 
library(survival)
d$S=Surv(d$surv,d$status)
(m1=survdiff(S~sex+cancer, data =d ))
(P=1-pchisq(m1$chisq,1))
(tit=paste0("Log Rank P value = ",signif(P,2)))
library(survMisc)
quartz(width=6,height=5)
theme_update(legend.position = c(.78, .82))
autoplot(survfit(S~cancer+sex, data =d ),xLab="Years",title=tit,survLineSize = 1,censSize = 2,axisLabSize = 12)
d$agec=cut(d$agedx,c(0,50,75,85,110))
quartz(width=6,height=5)
autoplot(survfit(S~agec, data =d ),xLab="Years",title=tit,survLineSize = 1,censSize = 2,axisLabSize = 12)
coxph(S~cancer*sex+agec,data=d)
d%>%group_by(cancer,sex)%>%summarize(agem=mean(agedx)) 

# mortality since diagnosis
dRA=d%>%filter(cancer=="RA")%>%select(yrdx,agedx,sex,surv,status)
head(dRA)
dRARS=d%>%filter(cancer=="RARS")%>%select(yrdx,agedx,sex,surv,status)
# library(demography)
# d=hmd.mx("USA", "username", "password") #make an account and put your info in here
# mrt=d$rate
# save(mrt,file="~/data/usMort/mrt.RData")
load("~/data/usMort/mrt.RData")
mrt$female[1:5,1:5]
brks=c(0,0.5,1,2,3,4,5,6,8) 
(DRA=msd(dRA,mrt,brks))
(DRARS=msd(dRARS,mrt,brks))
D=rbind(cbind(DRA,cancer="RA"),cbind(DRARS,cancer="RARS"))
quartz(width=6,height=5)
theme_update(axis.text=element_text(size=rel(1.4)),
             axis.title=element_text(size=rel(1.4)))
g=qplot(x=t,y=RR,data=D,col=sex,geom=c("line","point"),
        xlab="Years Since Dx of MDS",ylab="Relative Risk of Mortality")
g=g+geom_line(size=1)+facet_grid(cancer~.) 
g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15)) 

m=mrt$male
year=2000 # study starts 1/1/00
age=80 
lam=3*m[as.character(floor(age)),as.character(floor(year))]
p0=exp(-lam)
d=data.frame(yrdx=year,agedx=age,sex="male",
             status=sample(c(0,1),size=2e5,replace=T,prob=c(p0,1-p0)))
# Assuming exponentially distributed times to failure in 0 to 1 the average time to failure is
(aveT=((1-exp(-lam))/lam-exp(-lam))/(1-exp(-lam))) #~0.49  (use integration by parts to get this)
D=d%>%filter(status==1)%>%mutate(surv=aveT) 
r=rexp(n=5e7,rate=lam);R=r[r<1];mean(R) # (test formula for aveT)=> formula for aveT is OK
d=d%>%filter(status==0)%>%mutate(surv=1) # else lived the whole year
for (i in 1:9) {
  age=age+1
  year=year+1
  lam=3*m[as.character(floor(age)),as.character(floor(year))]
  p0=exp(-lam)
  n=dim(d)[1]
  d$status=sample(c(0,1),size=n,replace=T,prob=c(p0,1-p0))
  (aveT=((1-exp(-lam))/lam-exp(-lam))/(1-exp(-lam)))
  print(aveT)
  D=rbind(D,d%>%filter(status==1)%>%mutate(surv=surv+aveT))
  d=d%>%filter(status==0)%>%mutate(surv=surv+1)
}
dd=rbind(D,d)
head(dd,2)
(D=msd(dd,mrt,brks=0:9)) # validates msd 
D%>%summarize(O=sum(O),E=sum(E))%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
dd$S=Surv(dd$surv,dd$status==1)
quartz(width=6,height=5)
autoplot(survfit(S~sex, data =dd ),xLab="Years",title="One Sample",survLineSize = 1,censSize = 2,axisLabSize = 12)
###### END Survival chunk
# # search()
# detach("package:survMisc")
# detach("package:survival")
###########    RESTART R    ....  survMisc does weird stuff to code below (mk2D) that detach doesn't fix  ### 

###### START second cancer risks
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")#loads canc
levels(canc$cancer) # default is that AMLti and APL are separate from AML
#next chunk upgrades AML definition to include APL and AMLti (AML by translocation or inversion)
# mapCancs# shows ICD9/ICDO3 cancer definitions 
canc$cancer[canc$cancer=="APL"] ="AML" # overwrite back to AML
canc$cancer[canc$cancer=="AMLti"] ="AML" # overwrite back to AML
canc=canc%>%filter(cancer!="benign") #seqnums of 60-88 for benigns not handled yet so remove
canc$cancer=factor(canc$cancer)  # reset cancer field to eliminate extra levels 
levels(canc$cancer)
# trim down canc columns to bare neccesities needed for current project. 
canc=canc%>%select(-reg,-COD,-radiatn,-histo3,-ICD9)
load("~/data/SEER/mrgd/popsae.RData")#loads popsae (single ages extended to 85-99)
popsae=popsae%>%group_by(db,race,sex,age,year)%>%summarize(py=sum(py)) # sum on regs
(pf=seerSet(canc,popsae,Sex="female",ageStart=0,ageEnd=100)) #pooled (races) female seerSet
summary(pf) # breast and lung firsts are often irradiated
# pm=seerSet(canc,popsae,Sex="male",ageStart=0,ageEnd=100) #pooled (races) male seerSet
# summary(pm) # prostate cancer is often irradiated
pf=mk2D(pf,secondS=c("AML","MDS"))# list object pf goes in and also comes out, with more on it
pf  # smooths expected incidences into empty cells 
tail(pf$D,10)
brks=c(0,0.25,1,12)
pf=tsd(pf,brks=brks,trts=c("rad","noRad")) 
pf
mkDF(pf)[1:10,1:16]
mkExcel(pf,brks)

brks=c(0,0.25,0.5,0.75,1,1.5,2,2.5,3,4,5,6,8,10,12) # hi res too long to do in talk
brks=c(0,1,2,3,6,9,12)   # intermediate level resolution
pf=tsd(pf,brks=brks,trts=c("rad","noRad")) 
names(pf$L)
pf

df=mkDF(pf,brks)
head(df)
d=df%>%filter(cancer1=="breast")%>%select(cancer1,cancer2,trt,RR,rrL,rrU,t,int)
d$Radiation=factor(as.numeric(d$trt),labels=c("Yes","No"))
head(d)
graphics.off()
quartz(width=7,height=4)
theme_set(theme_gray(base_size = 16)) 
theme_update(legend.position = c(.8, .87))
g=qplot(x=t,y=RR,data=d,col=Radiation,geom=c("line","point"),
        xlab="Years Since Breast First Cancer",ylab="Relative Risk")
  g=g+facet_grid(cancer2~.,scales="free")+geom_abline(intercept=1, slope=0) #g=g+scale_y_continuous(breaks=1:10)
g=g+geom_errorbar(aes(ymin=rrL,ymax=rrU,width=.15))
print(g)

plot2D(pf) # Waits for input, so comment in scripts, else next line gets read in as that input


###### csd: 2nd cancer risks since diagnosis, with breaks/loops also on yrdx and agedx
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")#loads canc
canc$cancer[canc$cancer=="APL"] ="AML" # overwrite back to AML
canc$cancer[canc$cancer=="AMLti"] ="AML" # overwrite back to AML
canc=canc%>%filter(cancer!="benign") #seqnums of 60-88 for benigns not handled yet so remove
canc$cancer=factor(canc$cancer)  # reset cancer field to eliminate extra levels 
canc=canc%>%select(-reg,-COD,-radiatn,-histo3,-ICD9)
load("~/data/SEER/mrgd/popsae.RData")#loads popsae (single ages extended to 85-99)
popsae=popsae%>%group_by(db,race,sex,age,year)%>%summarize(py=sum(py)) # sum on regs
(pf=seerSet(canc,popsae,Sex="female",ageStart=0,ageEnd=100)) #pooled (races) female seerSet
pf=mk2D(pf,secondS=c("AML","MDS"))# list object pf goes in and also comes out, with more on it
brkst=c(0)
brksa=c(0)
pf2=csd(pf,brkst=c(0),brksy=1973,brksa=brksa,trts=c("rad")) 
mkExcel2(pf2,tsdn="b0")
