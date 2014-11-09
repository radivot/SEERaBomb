getE2=function(LPYM,D,ageStart,ageEnd,yearEnd,cancerS,picks) {
  EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
  EI=lapply(EI,function(x) acast(x, age~year, value.var="Eincid")) # convert 3 cols of df to matrices
#    EI=lapply(EI,function(x) x[,-1])
  if (ageStart>=1) LPYM=lapply(LPYM,function(x) x[-c(1:ageStart,(ageEnd+1):126),]) else
  LPYM=lapply(LPYM,function(x) x[-c((ageEnd+1):126),])
#   LPYM=lapply(LPYM,function(x) x[-c(1:15,86:126),-dim(x)[2]])
  LPYM[["CMML"]]=LPYM[["CMML"]][,-c(1:13)] # shave off early years when wasn't included in SEER
  LPYM[["MDS"]]=LPYM[["MDS"]][,-c(1:28)]
#    picks=names(EI)
  (E=matrix(0,nrow=length(cancerS),ncol=length(picks),dimnames=list(firstCanc=cancerS,secondCanc=picks)))
  for (i in cancerS) {   #i loop on first cancers
    PY=LPYM[[i]]
    for (j in picks) # j loop on second cancers
    { 
      if (i=="CMML" & !j%in%c("CMML","MDS"))  E[i,j]=sum(PY*EI[[j]][,as.character(1986:yearEnd)])
      if (i=="CMML" & j=="CMML")  E[i,j]=sum(PY*EI[[j]])
      if (i=="CMML" & j=="MDS" )  E[i,j]=sum(PY[,as.character(2001:yearEnd)]*EI[[j]])
      if (i=="MDS" & !j%in%c("CMML","MDS"))  E[i,j]=sum(PY*EI[[j]][,as.character(2001:yearEnd)])
      if (i=="MDS" & j=="CMML")  E[i,j]=sum(PY*EI[[j]][,as.character(2001:yearEnd)])
      if (i=="MDS" & j=="MDS" )  E[i,j]=sum(PY*EI[[j]])
      if (!i%in%c("CMML","MDS") & j=="MDS" )  E[i,j]=sum(PY[,as.character(2001:yearEnd)]*EI[[j]])
      if (!i%in%c("CMML","MDS") & j=="CMML" )  E[i,j]=sum(PY[,as.character(1986:yearEnd)]*EI[[j]])
      if (!i%in%c("CMML","MDS") & !j%in%c("CMML","MDS") )  E[i,j]=sum(PY*EI[[j]])
    }
  }   
  E/1e5
}

