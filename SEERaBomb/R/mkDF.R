mkDF<-function(seerSet) {
  age=trt=race=surv=year=py=popsa=cancer=NULL 
  if (is.null(seerSet$L)) stop("seerSet L field is empty. Please run tsd on your seerSet object!") else {
    print("Using 1st time series in L.") ### To move jth to 1st use seerSet$L=c(seerSet$L[j],seerSet$L[-j])."
    L=seerSet$L[[seerSet$active]]
  }
  secs=seerSet$secondS
  firstS=L$firstS
  trts=L$trtS
  d=NULL
  for (i in trts){
#         i=trts[1];library(reshape2)
    for (j in firstS){
#           j=firsts[1]
      tpts=melt(sapply(L[[i]]$PY,function(x) x[x$cancer1==j,"midPnt"]))
      O=melt(sapply(L[[i]]$Obs,function(x) x[j,secs]))
      E=melt(sapply(L[[i]]$Exp,function(x) x[j,secs]))
      d=rbind(d,data.frame(O=O$value,E=E$value,first=j,second=O$Var1,
                           trt=i,t=rep(tpts$value,each=length(secs)),sex=seerSet$sex,int=O$Var2))
    }
  }

  d
} 
