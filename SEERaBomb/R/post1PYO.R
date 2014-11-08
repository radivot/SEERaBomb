post1PYO=function(canc,brks=c(0,2,5),binIndx=1,Trt="rad" ) { 
  # to get rid of check notes. Using list inputs and with will shield these
  surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=seqnum=sex=race=reg=yrdx1=yrdiff=NULL 
  yrdx2=cancer1=cancer2=py=year=ageL=ageR=agedx1=NULL 
  if(sum(canc$trt==Trt)==0) stop(paste0("canc must have a trt column containing",Trt))
  binS=levels(cut(brks+0.1,breaks=c(brks,100)))
#   canc=canc%>%mutate(surv=round(surv/12,3),yrdx=round(yrdx+modx/12,3))%>%     
#     select(db,casenum:yrdx,radiatn,surv,cancer,trt)%>%
#     mutate(yrbrth=yrbrth+0.5,agedx=agedx+0.5)
  print(head(canc,2))
  bin=binS[binIndx]
  print(bin)
  (LL=getBinInfo(bin,binS)["LL"])
  #       D$cancer=factor(D$cancer) # get rid of opposite sex cancer types
  D0=canc%>%filter(seqnum==0,surv<200,surv>LL,trt==Trt)
  D1=canc%>%filter(seqnum==1,trt==Trt)%>%select(casenum,cancer,yrdx,agedx,trt)  
  D2=canc%>%filter(seqnum==2) # D2 holds second primaries
  D1=D1%>%filter(casenum%in%D2$casenum) # reduce firsts to just those with a second in D2 
  names(D1)[2:5]=c("cancer1","yrdx1","agedx1","trt1") #rename D1 cols so as not to join by them.
# D2 colnames  db  casenum	reg	race	sex	agedx	yrbrth	seqnum	modx	yrdx	radiatn	surv	cancer	trt
  D2=D2%>%select(casenum,cancer,yrdx,agedx,sex,race,yrbrth) # reduce D2 to cols we want to slap on 
#   D2=D2%>%select(casenum,cancer,yrdx,agedx) # reduce D2 to cols we want to slap on 
  names(D2)[2:4]=c("cancer2","yrdx2","agedx2") # and rename cols not to join by them
  D12=left_join(D2,D1,by="casenum") #Keeps all D2 rows, inserts missing where D1 doesn't match. 
  D12=D12%>%filter(!is.na(yrdx1)) # removes firsts before 1973
  D12=D12%>%mutate(yrdiff=cut(yrdx2-yrdx1,breaks=c(-1,brks,100),include.lowest = TRUE))
  D12=D12%>%filter(yrdiff==bin)
  PY0=D0%>%mutate(py=getPY(surv,bin,binS,brks),ageL=agedx+brks[binIndx],year=floor(yrdx+brks[binIndx])) 
  PY1=D12%>%mutate(py=getPY(yrdx2-yrdx1,bin,binS,brks),ageL=agedx1+brks[binIndx],year=floor(yrdx1+brks[binIndx])) 
  PY=rbind(PY0%>%mutate(cancer1=cancer,cancer2="none")%>%select(cancer1,cancer2,py,ageL,year),PY1%>%select(cancer1,cancer2,py,ageL,year))
  #       head(PY)
  n=dim(PY)[1]
  binMidPnt=LL+sum(PY$py)/n/2
  PY=PY%>%mutate(ageR=ageL+py)
  PYin=select(PY,-cancer1,-cancer2,-ageR)
  LPYin=split(PYin,PY$cancer1) #splitting done on first cancers, so resulting list names are of first cancers
  LPYinM=lapply(LPYin,as.matrix)
  LPYM=NULL
  # creat a matrix of zeros that is repeatedly the starting point of age-year PY accrual
  yrs=1973:2011
  ages=0.5:125.5
  Zs=matrix(0,ncol=length(yrs),nrow=length(ages))
  colnames(Zs)=yrs
  rownames(Zs)=ages
  head(Zs)
  for (i in names(LPYinM)) {
    PYM=Zs+0   # fake out system to allocate fresh memory for each instance of the matrix, i.e. each first cancer
    LPYM[[i]]=fillPYM(LPYinM[[i]],PYM)
  } 
  LD12=split(D12,D12$cancer1) # for getting observed cases in this interval later. Split on first => list names of firsts
#   lapply(LD12,function(x) table(x$cancer2)) #thyroid first yields 2 AML seconds and 25 thyroid
  O=t(sapply(LD12,function(x) table(x$cancer2)))
  list(LPYM=LPYM,O=O,binMidPnt=binMidPnt)
}



