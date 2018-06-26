simSurv<-function(d,mrt,rep=1,ltb=NULL,unif=TRUE){
  yrdx=agedx=sex=surv=status=P=NULL
  pullP=function(sex,age,year) {
    # sex="Male";age=10;year=1999
    # load("~/data/usMort/mrt.RData")#loads US mortality data
    M=mrt[[sex]]
    aN=dim(M)[1]
    yN=dim(M)[2]
    P=rep(0.5,aN)
    #need to slap on copies of final column to right for future of young 
    (Mfill=matrix(M[,yN],nrow=111,ncol=111))
    strtYr=max(as.numeric(colnames(M)))
    colnames(Mfill)=(strtYr+1):(strtYr+111)
    (Mbig=cbind(M,Mfill))
    rownames(Mbig)=c(0:109,"110")
    i=1
    for (k in 0:(aN-age-1))  {
      P[i]= Mbig[as.character(age+k),as.character(year+k)]
      i=i+1
    }
    P
  }
  
  simP=function(P) {
    for (k in 1:length(P)) 
      if(unif){ 
        if(runif(1)<P[k]) return(k-runif(1))
      } else {
        if(runif(1)<P[k]) return(k-0.5)
      }
  }
  # library(tidyverse)
  # load("~/data/SEER/mrgd/cancDef.RData")#load SEER cancer data
  # d=canc%>%filter(cancer=="CMML") #,agedx==99)
  # d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
  d=d%>%select(yrdx,agedx,sex,surv,status)#%>%mutate(ID=seq.int(nrow(d)))
  nd=d%>%select(-surv,-status) # these will be simulated in the new data
  # head(nd,2)
  nd=nd%>%mutate(P=pmap(list(sex,agedx,yrdx),pullP))
  # rep=5
  sim=function(P) replicate(rep, simP(P))
  nd=nd%>%mutate(surv=map(P,sim))
  # head(nd,2)
  D=nd%>%select(-P)
  D=D%>%unnest(surv)
  D$status=1
  D$type="simulated"
  # head(D,2)
  d$type="observed"
  d=rbind(d,D)
  # head(d,2)
  
  if (!is.null(ltb)) {
    getLT=function(sex,agedx,yrdx)  ltb[[as.character(sex)]][as.character(agedx),as.character(yrdx)]
    # getLT("Male",80,2000)
    ed=d
    ed$surv=mapply(getLT,d$sex,d$agedx,d$yrdx)
    ed$status=1
    ed$type="LT"
    d=rbind(d,ed)
  }
  d
}
