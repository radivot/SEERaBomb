mkDF<-function(seerSet) {
  age=trt=race=surv=year=py=popsa=cancer=ageE=ageEci=ageO=ageOci=NULL 
  if (is.null(seerSet$L)) stop("seerSet L field is empty. Please run tsd on your seerSet object!") else {
    cat(paste0("Using active time series in L.   Index:",seerSet$active,"\n")) 
    L=seerSet$L[[seerSet$active]]
  }
  secs=seerSet$secondS
  firstS=L$firstS
  trts=L$trtS
  d=NULL
  for (i in trts){
#                 i=trts[1];library(reshape2)
    for (j in firstS){
#                       j=firstS[1]
      print(j)
      tpts=melt(sapply(L[[i]]$PY,function(x) x[x$cancer1==j,"midPnt"]))
      n=melt(sapply(L[[i]]$PY,function(x) x[x$cancer1==j,"nFirsts"]))
      py=melt(sapply(L[[i]]$PY,function(x) x[x$cancer1==j,"PY"]))
#       AO=melt(sapply(L[[i]]$AgeO,function(x) x[x$cancer1==j,"age"]))
#       AOci=melt(sapply(L[[i]]$AgeO,function(x) x[x$cancer1==j,"ci95"]))
#       AE=melt(sapply(L[[i]]$AgeE,function(x) x[x$cancer1==j,"age"]))
#       AEci=melt(sapply(L[[i]]$AgeE,function(x) x[x$cancer1==j,"ci95"]))
      O=melt(sapply(L[[i]]$Obs,function(x) x[j,secs]))
      E=melt(sapply(L[[i]]$Exp,function(x) x[j,secs]))
      d=rbind(d,data.frame(int=O$Var2,sex=seerSet$sex,first=j,trt=i,
                           nFirsts=rep(n$value,each=length(secs)),
                           py=rep(py$value,each=length(secs)),
                           t=rep(tpts$value,each=length(secs)),
#                            ageE=rep(AE$value,each=length(secs)),
#                            ageEci=rep(AEci$value,each=length(secs)),
                           second=O$Var1,
#                            ageO=AO$value,
#                            ageOci=AOci$value,
                           O=O$value,E=E$value))
    }
  }
  d=d%>%mutate(RR=O/E,
#                aeL=ageE-ageEci,
#                aeU=ageE+ageEci,
#                aoL=ageO-ageOci,
#                aoU=ageO+ageOci,
               rrL=qchisq(.025,2*O)/(2*E),
               rrU=qchisq(.975,2*O+2)/(2*E)) #%>%select(-ageEci)
#                rrU=qchisq(.975,2*O+2)/(2*E))%>%select(-ageEci,-ageOci)
  i=sapply(d,is.numeric)
  d[i]=round(d[i],2)
  d
} 
