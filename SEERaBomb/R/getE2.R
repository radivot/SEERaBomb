getE2=function(LPYM,D) {
  EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
  EI=lapply(EI,function(x) acast(x, age~year, value.var="Eincid")) # convert 3 cols of df to matrices
  EI=lapply(EI,function(x) x[,-1])
  LPYM=lapply(LPYM,function(x) x[-c(1:15,85:126),-dim(x)[2]])
  LPYM[["CMML"]]=LPYM[["CMML"]][,-c(1:13)]
  LPYM[["MDS"]]=LPYM[["MDS"]][,-c(1:28)]
  picks=names(EI)
  (E=matrix(0,nrow=length(picks),ncol=length(picks),dimnames=list(firstCanc=picks,secondCanc=picks)))
  for (i in picks) {   #i loop on first cancers
    PY=LPYM[[i]]
    for (j in picks) # loop on second cancers
    { 
      if (i=="CMML" & !j%in%c("CMML","MDS"))  E[i,j]=sum(PY*EI[[j]][,as.character(1987:2011)])
      if (i=="CMML" & j=="CMML")  E[i,j]=sum(PY*EI[[j]])
      if (i=="CMML" & j=="MDS" )  E[i,j]=sum(PY[,as.character(2002:2011)]*EI[[j]])
      if (i=="MDS" & !j%in%c("CMML","MDS"))  E[i,j]=sum(PY*EI[[j]][,as.character(2002:2011)])
      if (i=="MDS" & j=="CMML")  E[i,j]=sum(PY*EI[[j]][,as.character(2002:2011)])
      if (i=="MDS" & j=="MDS" )  E[i,j]=sum(PY*EI[[j]])
      if (!i%in%c("CMML","MDS") & j=="MDS" )  E[i,j]=sum(PY[,as.character(2002:2011)]*EI[[j]])
      if (!i%in%c("CMML","MDS") & j=="CMML" )  E[i,j]=sum(PY[,as.character(1987:2011)]*EI[[j]])
      if (!i%in%c("CMML","MDS") & !j%in%c("CMML","MDS") )  E[i,j]=sum(PY*EI[[j]])
    }
  }   
  E/1e5
}

