getE=function(LPYM,L2D) { 
  getEinner=function(PYM,L2D) {
    #   getE=function(PYM) {
    #   oldPY=apply(PYM[86:126,],2,sum)
    #   PYM=rbind(PYM[-(86:126),],"90"=oldPY)
    PYM=PYM[-c(1:14,86:126),]
    newData=melt(PYM)
    names(newData)<-c("age","year","py")
    newData=cbind(newData)
    #   newData=cbind(newData,sex="female")
    picks=names(L2D)
    sapply(picks,function(x) sum(exp(predict(L2D[[x]],newData))) )
  } 
  ptm <- proc.time()
  EL=lapply(LPYM,getEinner,L2D)
  E=t(sapply(EL,"("))
  print(proc.time() - ptm) 
  E
}

