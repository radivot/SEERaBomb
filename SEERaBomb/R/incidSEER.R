incidSEER=function(canc,popsae,cancers) {
  cancer=sex=agedx=yrdx=year=age=py=cases=NULL
  
  # load("~/data/SEER/mrgd/popsae.RData") # loads in popsae (extended to ages 85-99)
  # load("~/data/SEER/mrgd/cancDef.RData") #loads in canc
  # library(tidyverse)
  # cancers="CMML"
  # cancers=c("CML","CMML")
  
  startYrs=c(CMML=1993,MDS=2001,NOS=2001,RA=2001,RAEB=2001,RARS=2001,Del5q=2001,LGL=2010)
  (startYrs=c(startYrs,MPN=2001,unknown=2001,AMLti=2001,LGL=2010))
  (outnms=setdiff(cancers,names(startYrs)))
  x=rep(1973,length(outnms))
  names(x)<-outnms
  (startYrs=c(startYrs,x))
  d=canc%>%select(cancer,sex,agedx,year=yrdx)%>%mutate(age=agedx+0.5)%>%filter(cancer%in%cancers)%>%select(-agedx)
  dcast(d%>%group_by(cancer,year)%>%summarize(cases=n()),cancer~year)
  
  d=d%>%filter(year>=startYrs[as.character(cancer)])
  dcast(d%>%group_by(cancer,year)%>%summarize(cases=n()),cancer~year)
  
  m=d%>%group_by(cancer,sex,age,year)%>%summarise(cases=n())
  # head(m%>%filter(cancer=="CMML"))
  p=popsae%>%group_by(sex,age,year)%>%summarise(py=sum(py))
  s=data.frame(sex=sort(unique(m$sex)))
  c=data.frame(cancer=cancers)
  cs=merge(c,s)
  options(warn=-1)
  pL=left_join(cs,p)
  d=left_join(pL,m)
  options(warn=0)
  # head(d%>%filter(cancer=="CMML"))
  d[is.na(d$cases),"cases"]=0 #join left missings where zero's should be, so fix this
  # d=d%>%filter(year>=startYrs[as.character(cancers)])
  head(d)
  d=d%>%group_by(cancer)%>%filter(year>=startYrs[as.character(cancer[1])])
  dcast(d%>%group_by(cancer,year)%>%summarize(cases=sum(cases)),cancer~year) #check, should be same
  head(d)  #OK, now collapse on years
  d=d%>%group_by(cancer,sex,age)%>%summarize(py=sum(py),cases=sum(cases))%>%mutate(py=py/1e5,incid=cases/py)
}
