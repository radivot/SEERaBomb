post1PYO=function(canc,brks=c(0,2,5),binIndx=1,Trt="rad",PYLong=FALSE,yearEnd ,firstS,secondS) { 
  # to get rid of check notes. Using list inputs and with will shield these
  #   surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=seqnum=sex=race=reg=yrdx1=yrdiff=NULL 
  #   surv=yrdx=db=casenum=cancer=trt=agedx=yrdx1=seqnum=yrdiff=NULL 
  #   yrdx2=yrdiffn=cancer1=cancer2=py=year=ageL=ageR=agedx1=NULL 
  surv=yrdx=age=casenum=cancer=trt=yrdx1=seqnum=yrdiff=NULL 
  age=NULL 
  yrdx2=yrdiffn=cancer1=cancer2=py=year=ageL=ageR=age1=age2=ageM=sem=NULL 
  if(sum(canc$trt==Trt,na.rm=T)==0) stop(paste0("canc must have a trt column containing",Trt))
  
  binS=levels(cut(brks+0.1,breaks=c(brks,100)))
  bin=binS[binIndx]
  #   print(bin)
  #   print("inPost")
  (LL=getBinInfo(bin,binS)["LL"])
#   canc=canc%>%filter()
  D0=canc%>%filter(seqnum==0,surv<200,surv>LL,trt==Trt,cancer%in%firstS)
  D0$cancer=factor(D0$cancer,levels=firstS) # get rid of levels not in firstS. 
  # need levels above since apparently pituitary is never irradiated
  D1=canc%>%filter(seqnum==1,trt==Trt,cancer%in%firstS)%>%select(casenum,cancer,yrdx,age,trt)  
  D1$cancer=factor(D1$cancer,levels=firstS) # get rid of levels not in firstS
  D2=canc%>%filter(seqnum==2) # D2 holds second primaries
  D1=D1%>%filter(casenum%in%D2$casenum) # reduce firsts to just those with a second in D2 
  names(D1)[2:5]=c("cancer1","yrdx1","age1","trt1") #rename D1 cols so as not to join by them.
  #   D2=D2%>%filter(casenum%in%D1$casenum) # no diff in simulation .. filt aft leftjoin took care of it 
  D2=D2%>%select(casenum,cancer2=cancer,yrdx2=yrdx,age2=age) # reduce D2 to cols we want to slap on 
#   names(D2)[2:4]=c("cancer2","yrdx2","age2") # and rename cols not to join by them
  #   print("inPost1")
  head(D1)
  head(D2)
  D12=left_join(D2,D1,by="casenum") #Keeps all D2 rows, inserts missing where D1 doesn't match. 
  #   print("inPost2")
  D12=D12%>%filter(!is.na(yrdx1)) # removes firsts before 1973
  D12=D12%>%mutate(yrdiffn=yrdx2-yrdx1)
  D12$yrdiffn[D12$yrdiffn==0]=0.33/12 # if first and second are in the same month, assume 1/3 of a month apart
  D12py=D12 #next 2-lines get cancers observed right, but PY of earlier intervals need to be delivered too
  D12=D12%>%mutate(yrdiff=cut(yrdiffn,breaks=c(-1,brks,100),include.lowest = TRUE)) 
  D12=D12%>%filter(yrdiff==bin) 
  PY0=D0%>%mutate(py=getPY(surv,bin,binS,brks),ageL=age+brks[binIndx],year=floor(yrdx+brks[binIndx])) 
  PY1=D12py%>%mutate(py=getPY(yrdiffn,bin,binS,brks),ageL=age1+brks[binIndx],year=floor(yrdx1+brks[binIndx])) 
  PYL=rbind(PY0%>%mutate(cancer1=cancer,cancer2="none")%>%select(cancer1,cancer2,py,ageL,year),
           PY1%>%select(cancer1,cancer2,py,ageL,year))
  N=dim(PYL)[1]
  binMidPnt=LL+sum(PYL$py)/N/2
  PYL=PYL%>%mutate(ageR=ageL+py,ageM=ageL+py/2)
  PYT=PYL%>%summarize(cases=n(),PY=sum(py),mean=mean(py),median=median(py),Q1=quantile(py,0.25),Q3=quantile(py,0.75))
  PY=PYL%>%group_by(cancer1)%>%summarize(cases=n(),PY=sum(py),mean=mean(py),median=median(py),
                                               Q1=quantile(py,0.25),Q3=quantile(py,0.75))
  AgeE=PYL%>%group_by(cancer1)%>%summarize(age=mean(ageM),sem=sd(ageM)/sqrt(n()),ci95=qt(0.975,n()-1)*sem,n=n())
  AgeO=D12%>%filter(cancer2%in%secondS)%>%group_by(cancer1,cancer2)%>%
    summarize(age=mean(age2),sem=sd(age2)/sqrt(n()),ci95=qt(0.975,n()-1)*sem,n=n())
#   AgeE=PY%>%group_by(cancer1,cancer2)%>%
#     summarize(age=mean(ageL)+binMidPnt,L=t.test$conf.int[1],L=t.test(ageL)$conf.int[2])%>%filter(cancer2%in%secondS)
#   AgeO=PY%>%filter(cancer2%in%secondS)%>%group_by(cancer1,cancer2)%>%
#     summarize(age=mean(ageR),L=t.test(ageR)$conf.int[1],L=t.test$conf.int[2])%>%filter(cancer2%in%secondS)
  
  PYin=PYL%>%select(-cancer1,-cancer2,-ageR)
  LPYin=split(PYin,PYL$cancer1) #splitting done on first cancers, so resulting list names are of first cancers
  LPYinM=lapply(LPYin,as.matrix)
  LPYM=NULL
  # creat a matrix of zeros that is repeatedly the starting point of age-year PY accrual
  yrs=1973:yearEnd
  nyears=length(yrs)
  ages=0.5:125.5
  Zs=matrix(0,ncol=nyears,nrow=length(ages))
  colnames(Zs)=yrs
  rownames(Zs)=ages
  #     head(Zs)
  #   print(tail(D12,2))
  #   print(Trt)
  print(bin)
  #   print(sapply(LPYinM,dim))
  #   print(dim(sapply(LPYinM,dim)))
  
  #   #  print(gc())
  #   for (i in names(LPYinM)) {
  for (i in firstS) {
    PYMat=Zs+0   # fake out system to allocate fresh memory for each instance of the matrix, i.e. each first cancer
    #       print(i)
    #       print(tail(LPYinM[[i]]))
    #     print(dim(LPYinM[[i]]))
    #       print(head(LPYinM[[i]],2))
    #      print(tail(LPYinM[[i]]))
    #     print(head(PYM))
    #         if(dim(LPYinM[[i]])[1]==0) LPYM[[i]]=as.matrix(data.frame(py=c(.1,.1),ageL=50,year=2000)) # set up dummy matrix
    LPYM[[i]]=fillPYM(LPYinM[[i]],PYMat)
    #          LPYM[[i]]=mkPYM(LPYinM[[i]],nyears)
    #         LPYM[[i]]=mkPYMR(LPYinM[[i]],yearEnd)
    #     print(head(LPYM[[i]]))
    #       rm(PYM) 
    #       gc()
  } 
  #    LPYM=lapply(LPYinM,mkPYM,nyears)
  #   LPYM=lapply(LPYinM,mkPYMR,yearEnd)
  #   print("doneLapply")
  
  LD12=split(D12,D12$cancer1) # for getting observed cases in this interval later. Split on first => list names of firsts
  #   lapply(LD12,function(x) table(x$cancer2)) #thyroid first yields 2 AML seconds and 25 thyroid
  O=t(sapply(LD12,function(x) table(x$cancer2)))
  rownames(O)=names(LD12)
  colnames(O)=levels(D12$cancer2)
  #   print("here")
  if (PYLong) return(list(LPYM=LPYM,O=O,binMidPnt=binMidPnt,AgeE=AgeE,AgeO=AgeO,PY=PY,PYT=PYT,PYL=PYL))  # no speed gain with this
  else return(list(LPYM=LPYM,O=O,binMidPnt=binMidPnt,AgeE=AgeE,AgeO=AgeO,PY=PY,PYT=PYT))
}



