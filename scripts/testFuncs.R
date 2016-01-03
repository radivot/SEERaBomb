library(devtools); install_github("radivot/SEERaBomb",subdir="SEERaBomb")
library(SEERaBomb)
rm(list=ls()) 
library(dplyr)
library(reshape2) 
library(inline)
(brks=c(0,0.25,1,3,5))  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsx interval/row names 
getBinInfo(binS[3],binS) # test getBinInfo
getBinInfo(binS[1],binS)

brks=c(0,0.25,1,3,6)  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #make a vector of intervals 
survTimes=c(8,16,1.5,3.7)
getPY(survTimes,binS[1],binS,brks)# all contribute 0.25 to first interval 
getPY(survTimes,binS[4],binS,brks)# 3rd and 4th survivals contribute 0 and 0.7 to (3,6]
getPY(survTimes,binS[5],binS,brks)# 1st and 2nd survivals contribute 2 and 10 years to (6,100]

(mats=matrix(0,ncol=20,nrow=70))
(PYtest=structure(c(3.5, 11.25, 51.5, 58.5, 1974, 1976, 55, 69.75),.Dim = c(2L,4L), 
                  .Dimnames=list(c("1","10"),c("py", "ageL", "year", "ageR"))))
fillPYM(PYtest, mats)

# first run mkRRtsx (in examples) up to saving pm5 to a file, which is loaded here
system.time(load("~/Results/pm5.RData")) # 1 secs to load. 
#make sure year and age limits are right
range(pm5$D$age)
range(pm5$canc$age)
range(pm5$popsa$age)
range(pm5$D$year)
range(pm5$canc$year)
range(pm5$popsa$year)
#see what things look like MDS and CMML
library(rehape2)
D=pm5$D
EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
EI=lapply(EI,function(x) acast(x, age~year, value.var="Eincid")) # convert 3 cols of df to matrices
EI[["MDS"]]
EI[["CMML"]]

L=post1PYO(pm5$canc,brks=c(0,2,5),binIndx=1,Trt="rad" )
O=L$O
L$LPYM[["prostate"]]
L$LPYM[["MDS"]]
L$LPYM[["CMML"]]
E=getE2(L$LPYM,pm5$D) # make sure matrices are comformable
O/E



# chunk to show that pickFields now catches field name typos and wrong orderings 
library(SEERaBomb)
sas=getFields()
picksDef=c("casenum","reg","race","sex","agedx","yrbrth",
           "seqnum","modx","yrdx","histo3","radiatn","recno",
           "agerec","ICD9","numprims","COD","surv")
pickMin=c("race","sex","agedx","histo3","radiatn","agerec","ICD9")
pickMin=c("sex","agedx","histo3","radiatn","agerec","ICD9")
pickFields(sas, pickMin) # missing reg
pickTypo=c("reg","race","sex","agedx","histo3","radiatn","agerec","ICD9","CD","srv")
pickFields(sas, pickTypo) # last two not on list
pickBadOrd=c("reg","race","sex","agedx","ICD9","histo3","radiatn","agerec")
pickBadOrd=c("reg","race","radiatn","sex","agedx","ICD9","histo3","agerec")
pickFields(sas, pickBadOrd)


# chunk to test msd()
rm(list=ls())
# library(demography)
# d=hmd.mx("USA", "username", "password") #make an account and put your info in here
# mrt=d$rate
# save(mrt,file="~/data/usMort/mrt.RData")
load("~/data/usMort/mrt.RData")
library(dplyr)
m=mrt$male
year=2000 # study starts 1/1/00
age=80 
lam=5*m[as.character(floor(age)),as.character(floor(year))]
p0=exp(-lam)
d=data.frame(yrdx=year,agedx=age,sex="male",
             status=sample(c(0,1),size=2e5,replace=T,prob=c(p0,1-p0)))
# D=d%>%filter(status==1)%>%mutate(surv=0.5) # if dead that year, on average dead half-way through. 
# Or be more precise assuming exponentially distributed times to failure in 0 to 1
(aveT=((1-exp(-lam))/lam-exp(-lam))/(1-exp(-lam))) #~0.49  (use integration by parts to get this)
D=d%>%filter(status==1)%>%mutate(surv=aveT) 
# r=rexp(n=5e7,rate=lam) # (test formula for aveT)        
# R=r[r<1]
# mean(R)  # => formula for aveT is OK
d=d%>%filter(status==0)%>%mutate(surv=1) # else lived the whole year
for (i in 1:9) {
  age=age+1
  year=year+1
  lam=5*m[as.character(floor(age)),as.character(floor(year))]
  p0=exp(-lam)
  # if (i>=0) p0=exp(-5*lam)
  n=dim(d)[1]
  d$status=sample(c(0,1),size=n,replace=T,prob=c(p0,1-p0))
  (aveT=((1-exp(-lam))/lam-exp(-lam))/(1-exp(-lam)))
  print(aveT)
  D=rbind(D,d%>%filter(status==1)%>%mutate(surv=surv+aveT))
  d=d%>%filter(status==0)%>%mutate(surv=surv+1)
}

dd=rbind(D,d)
# debug(msd)
# debug(post1PYOm)
brks=0:9 #c(0,5)
library(SEERaBomb)
(D=msd(dd,mrt,brks))

D%>%summarize(O=sum(O),E=sum(E))%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
head(dd)
dd$S=Surv(dd$surv,dd$status==1)
library(survMisc)
quartz(width=6,height=5)
theme_update(legend.position = c(.78, .82))
autoplot(survfit(S~sex, data =dd ),xLab="Years",title="One Sample",survLineSize = 1,censSize = 2,axisLabSize = 12)
