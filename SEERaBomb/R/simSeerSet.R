simSeerSet<-function(N=2e9,yearEnd=2012,ka=1e-5,kb=0.04,Ab=1e-5,Rba=1e-3,delay=1,period=4,shape=4) {
  #   agedx=age=age86=canc=yrdx=sex=race=surv=modx=yrbrth=NULL 
  cancers=NULL 
  data(stdUS)
  if (shape<1) {
    shape=1
    print("Warning: shape must be 1 or higher. It was less and is being set to 1. ")
  }
#   N=2e9;yearEnd=2012;ka=1e-5;kb=0.04;Ab=1e-5;Rba=1e-3;delay=1;period=4;shape=1
  popsa=merge(data.frame(age=0.5:99.5),data.frame(year=1973:yearEnd))
  popsa$py=N/40*SEERaBomb::stdUS$prop[round(popsa$age+0.5)]
  #   sum(popsa$py)
  head(popsa)
  #   tail(popsa)
  A=cbind(popsa[,1:2],cancers=rpois(dim(popsa)[1],ka*popsa$age*popsa$py))
  head(A)
  cancA=A[rep(seq_len(nrow(A)), times=A$cancers),]%>%select(-cancers)
  head(cancA,10)
  # the approach in this chunk is way too slow
  #   A=A%>%group_by(year,age) 
  #   myGrow=function(d) {
  #     D=data.frame(NULL)
  #     if (d$cancers>0) for (i in 1:d$cancers) D=rbind(D,d)
  #     D   }
  #   cancA=A%>%do(myGrow(.))%>%select(-cancers)
  cancA$surv=rexp(dim(cancA)[1],rate=0.2)
  #   mean(canc$surv)
  #   median(canc$surv)
  cancA$cancer="A"
  cancA$yrdx=cancA$year+runif(dim(cancA)[1],max=0.9999)
  cancA$casenum=1:dim(cancA)[1]
  cancA$seqnum=0
  py=cancA$surv
  head(py,40)
  py=ifelse(py<delay,0,py-delay)
  py=ifelse(py>period,period,py)
  #    cancA$seconds=rpois(dim(cancA)[1],Rba*py)
  cancA$seconds=rpois(dim(cancA)[1],5*Ab*exp(kb*(cancA$age+(py+delay)/2))*py) #induced is 500x > background risk
  sum(cancA$seconds)
  cancA$seqnum[cancA$seconds>0]=1
  cancBA=cancA[cancA$seconds>0,]
  cancBA$seqnum=2
  cancBA$cancer="B"
  #   cancBA$surv=delay+period+1 # big enough to die after the cancer
  #   cancBA$yrdx=cancBA$yrdx+delay+period*rbeta(dim(cancBA)[1],shape,shape)
  #   cancBA$yrdx=cancBA$yrdx+runif(dim(cancBA)[1],delay,delay+period)
  #   cancBA$yrdx=cancBA$yrdx+pmin(runif(dim(cancBA)[1],delay,delay+period),cancBA$surv)
  cancBA$yrdx=cancBA$yrdx+pmin(delay+period*rbeta(dim(cancBA)[1],shape,shape),cancBA$surv)
  cancBA$seconds=NULL
  cancA$seconds=NULL
  #   hist(rbeta(1e4,1,1))
  #   hist(rbeta(1e4,2,2))
  #    hist(rbeta(1e4,4,4))
  #    hist(rbeta(1e4,40,40))
  #     hist(rbeta(1e4,0,0))
  #   head(cancBA)
  #   head(cancA[60:65,])
  cancA=rbind(cancA,cancBA)
  
  B=cbind(popsa[,1:2],cancers=rpois(dim(popsa)[1],Ab*exp(kb*popsa$age)*popsa$py))
  cancB=B[rep(seq_len(nrow(B)), times=B$cancers),]%>%select(-cancers)
  cancB$surv=rexp(dim(cancB)[1],rate=0.5)
  cancB$cancer="B"
  cancB$yrdx=cancB$year+runif(dim(cancB)[1],max=0.9999)
  cancB$casenum=1:dim(cancB)[1]
  cancB$seqnum=0
  
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
  seerSet=mk2D(seerSet,secondS="B") 
  
  
  cancA$seconds=rpois(dim(cancA)[1],Ab*exp(kb*(cancA$age+(cancA$surv)/2))*cancA$surv) 
  sum(cancA$seconds)
  cancBA=cancA[cancA$seconds>0,]
  cancBA$seqnum=2
  cancBA$cancer="B"
  cancBA$yrdx=cancBA$yrdx+cancBA$surv*rbeta(dim(cancBA)[1],shape,shape)
  cancBA$seconds=NULL
  cancBA$trt="noRad"
  head(cancBA)
  head(seerSet$canc)
  seerSet$canc=rbind(seerSet$canc,cancBA)
  seerSet
} # return a list that can be attached or with-ed in other functions
