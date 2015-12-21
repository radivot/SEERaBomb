post1PYOm=function(D,brks=c(0,2,5),binIndx=1) { survC=surv=agedx=py=year=ageL=NULL 
  yearEnd=max(D$yrdx)
  binS=levels(cut(brks+0.1,breaks=c(brks,100)))
  bin=binS[binIndx]
  LL=getBinInfo(bin,binS)["LL"]
  D=D%>%mutate(survC=cut(surv,breaks=c(-1,brks,100),include.lowest = TRUE)) 
  D=D%>%filter(survC==bin) 
  O=sum(D$status)
  D=D%>%mutate(py=getPY(surv,bin,binS,brks)) # getpy leaves zeros when surv end is left of LL
  D=D%>%filter(py>0)  #get rid of such rows upfront
  D=D%>%mutate(ageL=agedx+0.5+LL) 
  D$year=floor(D$yrdx+LL)
  D=D%>%select(py,ageL,year)
  binMidPnt=LL+sum(D$py)/dim(D)[1]/2
  PYin=as.matrix(D)
  yrs=1973:yearEnd; ages=0.5:125.5  # this + next line = initiate PYM with zeros
  PYM=matrix(0,ncol=length(yrs),nrow=length(ages),dimnames=list(ages,yrs)) 
  fillPYM(PYin,PYM)
  list(PYM=PYM,O=O,binMidPnt=binMidPnt)
}



