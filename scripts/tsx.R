rm(list=ls()) 
source("~/case/active/seer/scripts/funcs.R")
library(dplyr)
library(reshape2)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/seer/mrgd/popsa.RData") # use single-age (sa) py data
head(canc,2)
ageCut=15
canc=canc%.%filter(agedx>=ageCut,agedx<85)
popsa=popsa%.%filter(age86>=ageCut,age86<85)


# table(canc$agedx)
(brks=c(1,3,5,7,10,13,16,20))  
(brks=c(1))  # total RR for all tsx > 1 year
(brks=c(1,12))  
(brks=c(2,5,10,15))  
(brks=c(0,0.5,1,2,5,10,15))  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsx interval/row names 


getO=function(D12) {
  tmp3=D12%.%filter(cancer2%in%picks)
  print(n<-dim(tmp3)[1])
  if (n==0) return(zpicks) else {
    tmp2=tmp3%.%group_by(cancer2)
    tmp=tmp2%.%summarise(cases=n())
    rownames(tmp)=tmp$cancer2
    tmp=tmp[picks,"cases"] # reorder rows to match picks order
    names(tmp)=picks
    #     print(tmp)
    return(ifelse(is.na(tmp),0,tmp))
  }
}


getE=function(PYM) {
  #   oldPY=apply(PYM[86:126,],2,sum)
  #   PYM=rbind(PYM[-(86:126),],"90"=oldPY)
  PYM=PYM[-c(1:14,86:126),]
  newData=melt(PYM)
  names(newData)<-c("age","year","py")
  newData=cbind(newData)
  #   newData=cbind(newData,sex="female")
  sapply(picks,function(x) sum(exp(predict(L2D[[x]],newData))) )
} 

ptm <- proc.time()

for (Sex in c("male","female")) {
  
  cancS=canc%.%filter(sex==Sex)
  popsaS=popsa%.%filter(sex==Sex)
  
  
  cancS=cancS%.%mutate(surv=round(surv/12,3),yrdx=round(yrdx+modx/12,3))%.%     
    select(db,casenum:yrdx,radiatn,surv,cancer)%.%
    mutate(yrbrth=yrbrth+0.5,agedx=agedx+0.5)
  head(cancS,2)
  
  yrs=1974:2012
  ages=0.5:125.5
  Zs=matrix(0,ncol=length(yrs),nrow=length(ages))
  colnames(Zs)=yrs
  rownames(Zs)=ages
  head(Zs)
  
  # bring in L2D
  load(paste0("~/ccf/tomR/",ifelse(Sex=="male","M","F"),"2D",ageCut,".RData")) 
  # picks, zpicks and L2D get passed globally to these since they are used in lapply() below
  picks=names(L2D)
  zpicks=rep(0,length(picks))
  names(zpicks)=picks
  zpicks
  
  L=list()
  for (R in c("0","16")) 
  { 
    #           R="0"
    mids=NULL
    Obs=NULL
    Exp=NULL
    for (bin in binS) 
    {
      #                 (      bin=binS[1])
      # 7=refused, 5=NOS, 6=other rad 73-87 (other=other than beam=>my pref below)
      if (R=="0") D=cancS%.%mutate(rad=as.numeric((radiatn==0)|(radiatn==7))) else
        if (R=="16") D=cancS%.%mutate(rad=as.numeric((radiatn>=1)&(radiatn<=6))) 
      LL=getBinInfo(bin,binS)$LL
      D$cancer=factor(D$cancer) # get rid of opposite sex cancer types
      D0=D%.%filter(seqnum==0,surv<200,surv>LL,rad==1)
      D1=D%.%filter(seqnum==1,rad==1)%.%select(casenum,cancer,yrdx,agedx,rad)  
      D2=D%.% filter(seqnum==2) # D2 holds second primaries
      D1=D1%.%filter(casenum%in%D2$casenum) # reduce firsts to just those with a second in D2 
      names(D1)[2:5]=c("cancer1","yrdx1","agedx1","rad1") #rename D1 cols so as not to join by them.
      D2=D2%.%select(casenum,cancer,yrdx,agedx,sex,race,yrbrth,reg,db) # reduce D2 to cols we want to slap on 
      names(D2)[2:4]=c("cancer2","yrdx2","agedx2") # and rename cols not to join by them
      D12=left_join(D2,D1,by="casenum") #Keeps all D2 rows, inserts missing where D1 doesn't match. 
      D12=D12%.%filter(!is.na(yrdx1)) # removes firsts before 1973
      D12=D12%.%mutate(yrdiff=cut(yrdx2-yrdx1,breaks=c(-1,brks,100),include.lowest = TRUE))
      D12=D12%.%filter(yrdiff==bin)
      indx=getBinInfo(bin,binS)$indx
      PY0=D0%.%mutate(py=getPY(surv,bin,binS,brks),ageL=agedx+brks[indx],year=floor(yrdx+brks[indx])) 
      PY1=D12%.%mutate(py=getPY(yrdx2-yrdx1,bin,binS,brks),ageL=agedx1+brks[indx],year=floor(yrdx1+brks[indx])) 
      PY=rbind(PY0%.%mutate(cancer1=cancer,cancer2="none")%.%select(cancer1,cancer2,py:year),PY1%.%select(cancer1,cancer2,py:year))
      head(PY)
      n=dim(PY)[1]
      mids=c(mids,LL+sum(PY$py)/n/2)
      PY=PY%.%mutate(ageR=ageL+py)
      PYin=select(PY,-cancer1,-cancer2,-ageR)
      LPYin=split(PYin,PY$cancer1)
      LPYin=lapply(LPYin,as.matrix)
      LPYM=NULL
      for (i in names(LPYin)) {
        PYM=Zs+0
        LPYM[[i]]=fillPYM(LPYin[[i]],PYM)
      } 
      EL=lapply(LPYM,getE)
      Exp[[bin]]=t(sapply(EL,"("))
      LD12=split(D12,D12$cancer1)
      OL=lapply(LD12,getO)
      Obs[[bin]]=t(sapply(OL,"("))
      #          Obs[[bin]]=t(sapply(OL,as.matrix))
      #         Obs[[bin]]=OL
    } # loop on tsx bins
    L[[R]]$mids=mids
    L[[R]]$Obs=Obs
    L[[R]]$Exp=Exp
  } # loop on R
  save(L,file=paste0("/Users/radivot/ccf/TOMR/",ifelse(Sex=="male","M","F"),"2D",paste(brks,collapse="_"),".RData") )
} # Sex loop
proc.time() - ptm
# L
