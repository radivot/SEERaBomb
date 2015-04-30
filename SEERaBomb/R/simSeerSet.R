simSeerSet<-function(N=2e9,yearEnd=2012,ka=1e-5,kb=0.04,Ab=1e-5,Rba=1e-3,delay=1,period=4,shape=1) {
  #   agedx=age=age86=canc=yrdx=sex=race=surv=modx=yrbrth=NULL 
  cancers=NULL 
  data(stdUS)
  if (shape<1) {
    shape=1
    print("Warning: shape must be 1 or higher. It was less and is being set to 1. ")
  }
#   N=2e9;yearEnd=2012;ka=1e-5;kb=0.04;Ab=1e-5;Rba=1e-3;delay=1;period=4;shape=1;library(dplyr)
  popsa=merge(data.frame(age=0.5:99.5),data.frame(year=1973:yearEnd))
  popsa$py=N/40*SEERaBomb::stdUS$prop[round(popsa$age+0.5)]
  #   sum(popsa$py)
  head(popsa)
  #   tail(popsa)
  A=cbind(popsa[,1:2],cancers=rpois(dim(popsa)[1],ka*popsa$age*popsa$py))
  head(A)
  sum(A$cancers)
  cancA=A[rep(seq_len(nrow(A)), times=A$cancers),]%>%select(-cancers)
  head(cancA,10)
  # the approach in this chunk is way too slow
  #   A=A%>%group_by(year,age) 
  #   myGrow=function(d) {
  #     D=data.frame(NULL)
  #     if (d$cancers>0) for (i in 1:d$cancers) D=rbind(D,d)
  #     D   }
  #   cancA=A%>%do(myGrow(.))%>%select(-cancers)
  #   cancA$surv=rexp(dim(cancA)[1],rate=0.1)
  cancA$surv=runif(dim(cancA)[1],0,20)
  #   mean(canc$surv)
  #   median(canc$surv)
  cancA$cancer="A"
  cancA$yrdx=cancA$year+runif(dim(cancA)[1],max=0.9999)
  cancA$casenum=1:dim(cancA)[1]
  cancA$seqnum=0
  
  
  ######first treatment induced B cases
  py=cancA$surv
  head(py,40)
  py=ifelse(py<=delay,0,py-delay)
  py=ifelse(py>=period,period,py)
  sum(py==0)
  #   subCancA=cancA[py>0,]
  #   py=py[py>0,]
  #   head(py,40)
  pyi=floor(py)
  #   sum(pyi==1)
  pyr=py-pyi
  #   head(pyi,40)
  #   head(pyr,40)
  #    cancA$seconds=rpois(dim(cancA)[1],Rba*py)
  #   cancA$seconds=rpois(dim(cancA)[1],5*Ab*exp(kb*(cancA$age+(py+delay)/2))*py) #induced is 5x > background risk
  myexpi=function(age1,pyi1) sum(exp(kb*(age1+0.5+delay + 0:(pyi1-1))))
  expis=mapply(myexpi,cancA$age,pyi)
  myexpr=function(age1,pyi1,pyr1) pyr1*exp(kb*(age1+delay+pyi1+pyr1/2))
  exprs=mapply(myexpr,cancA$age,pyi,pyr)
  cancA$seconds=rpois(dim(cancA)[1],5*Ab*(expis+exprs) ) 
  sum(cancA$seconds)
  table(cancA$seconds,cut(py,breaks=seq(0,4,0.1)))
  cancA$seqnum[cancA$seconds>0]=1
  cancBA=cancA[cancA$seconds>0,]
  cancBA$seqnum=2
  cancBA$cancer="B"
  #   cancBA$yrdx=cancBA$yrdx+delay+period*rbeta(dim(cancBA)[1],shape,shape)
  #   cancBA$yrdx=cancBA$yrdx+pmin(runif(dim(cancBA)[1],delay,delay+period),cancBA$surv)
  #   cancBA$yrdx=cancBA$yrdx+runif(dim(cancBA)[1],delay+1e-4,delay+period-1e-4)
  cancBA$yrdx=cancBA$yrdx+runif(dim(cancBA)[1],delay,     delay+period)
  cancBA$seconds=NULL
  cancA$seconds=NULL
  #   hist(rbeta(1e4,1,1))
  #    hist(rbeta(1e4,4,4))
  cancA=rbind(cancA,cancBA)
  
  ######now background B after A cases
  py=cancA$surv
  pyi=floor(py)
  pyr=py-pyi
  expis=mapply(myexpi,cancA$age,pyi)
  exprs=mapply(myexpr,cancA$age,pyi,pyr)
  cancA$seconds=rpois(dim(cancA)[1],Ab*(expis+exprs) ) 
#   sum(cancA$seconds)
#   table(cancA$seconds,cut(py,breaks=seq(0,4,0.1)))
  cancA$seqnum[cancA$seconds>0]=1
  cancBA=cancA[cancA$seconds>0,]
  cancBA$seqnum=2
  cancBA$cancer="B"
  cancBA$yrdx=cancBA$yrdx+runif(dim(cancBA)[1],0,py)
  cancBA$seconds=NULL
  cancA$seconds=NULL
# head(cancBA)
# head(cancA)
  cancA=rbind(cancA,cancBA)
  
  ######now background B
  B=cbind(popsa[,1:2],cancers=rpois(dim(popsa)[1],Ab*exp(kb*popsa$age)*popsa$py))
  cancB=B[rep(seq_len(nrow(B)), times=B$cancers),]%>%select(-cancers)
  cancB$surv=rexp(dim(cancB)[1],rate=0.5)
  cancB$cancer="B"
  cancB$yrdx=cancB$year+runif(dim(cancB)[1],max=0.9999)
  cancB$casenum=1:dim(cancB)[1]
  cancB$seqnum=0
  
  ######  merge A and B
  canc=rbind(cancA,cancB)
  canc$trt="noRad"
  canc$trt=factor(canc$trt)
  canc$cancer=factor(canc$cancer)
  head(canc)
  cancerS=levels(canc$canc)
  #   sapply(canc,class)
  canc=tbl_df(canc)
  popsa=tbl_df(popsa)
  # and package it all up
  seerSet=list(canc=canc,popsa=popsa,ageStart=min(popsa$age),ageEnd=max(popsa$age),sex="neut",race="neut",
               cancerS=cancerS,yearEnd=max(popsa$year))
  class(seerSet)="seerSet"
  seerSet
} # return a list that can be attached or with-ed in other functions
