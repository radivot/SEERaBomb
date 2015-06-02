getE=function(LPYM,D,ageStart,ageEnd,yearEnd,cancerS,secondS) {
  EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
  EI=lapply(EI,function(x) acast(x, age~year, value.var="Eincid")) # convert 3 cols of df to matrices
#    EI=lapply(EI,function(x) x[,-1])
  if (ageStart>=1) LPYM=lapply(LPYM,function(x) x[-c(1:ageStart,(ageEnd+1):126),]) else
  LPYM=lapply(LPYM,function(x) x[-c((ageEnd+1):126),])
#   LPYM=lapply(LPYM,function(x) x[-c(1:15,86:126),-dim(x)[2]])
  LPYM[["CMML"]]=LPYM[["CMML"]][,-c(1:13)] # shave off early years when wasn't included in SEER
  LPYM[["MDS"]]=LPYM[["MDS"]][,-c(1:28)]
  LPYM[["MPN"]]=LPYM[["MPN"]][,-c(1:28)]
#    secondS=names(EI)
  (E=matrix(0,nrow=length(cancerS),ncol=length(secondS),dimnames=list(firstCanc=cancerS,secondCanc=secondS)))
  for (i in cancerS) {   #i loop on first cancers
    PY=LPYM[[i]]
    for (j in secondS) # j loop on second cancers
    { 
      if (i=="CMML" & !j%in%c("CMML","MDS","MPN"))  E[i,j]=sum(PY*EI[[j]][,as.character(1986:yearEnd)])
      if (i=="CMML" & j=="CMML")  E[i,j]=sum(PY*EI[[j]])
      if (i=="CMML" & j%in%c("MDS","MPN"))  E[i,j]=sum(PY[,as.character(2001:yearEnd)]*EI[[j]])
      if (i%in%c("MDS","MPN") & !j%in%c("CMML","MDS","MPN"))  E[i,j]=sum(PY*EI[[j]][,as.character(2001:yearEnd)])
      if (i%in%c("MDS","MPN") & j=="CMML")  E[i,j]=sum(PY*EI[[j]][,as.character(2001:yearEnd)])
      if (i%in%c("MDS","MPN") & j%in%c("MDS","MPN") )  E[i,j]=sum(PY*EI[[j]])
      if (!i%in%c("CMML","MDS","MPN") & j%in%c("MDS","MPN") )  E[i,j]=sum(PY[,as.character(2001:yearEnd)]*EI[[j]])
      if (!i%in%c("CMML","MDS","MPN") & j=="CMML" )  E[i,j]=sum(PY[,as.character(1986:yearEnd)]*EI[[j]])
      if (!i%in%c("CMML","MDS","MPN") & !j%in%c("CMML","MDS","MPN") )  E[i,j]=sum(PY*EI[[j]])
    }
  }   
  E/1e5
}

