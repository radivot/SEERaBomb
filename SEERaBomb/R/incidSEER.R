incidSEER=function(canc,popsae,cancers) {
  cancer=sex=race=agedx=yrdx=year=age=py=cases=NULL
  
  # load("~/data/SEER/mrgd/popsae.RData") # loads in popsae (extended to ages 85-99)
  # load("~/data/SEER/mrgd/cancDef.RData") #loads in canc
  # library(tidyverse)
  # cancers="CMML"
  # cancers=c("CML","CMML")
  
  startYrs=c(CMML=1993,MDS=2001,NOS=2001,RA=2001,RAEB=2001,RARS=2001,Del5q=2001,LGL=2010)
  (startYrs=c(startYrs,MPN=2001,unknown=2001,AMLti=2001,LGL=2010))
  (outnms=setdiff(cancers,names(startYrs)))
  x=rep(1975,length(outnms))
  names(x)<-outnms
  (startYrs=c(startYrs,x))
  D=canc%>%select(cancer,sex,race,agedx,year=yrdx)%>%mutate(age=agedx+0.5)%>%filter(cancer%in%cancers)%>%select(-agedx)
  # reshape2::dcast(d%>%group_by(cancer,year)%>%summarize(n=n()),cancer~year)

  m=D%>%group_by(cancer,sex,race,age,year)%>%summarise(n=n())
  p=popsae%>%group_by(sex,race,age,year)%>%summarise(py=sum(py))
  s=data.frame(sex=sort(unique(m$sex)))
  # r=data.frame(race=sort(unique(m$race))) # skip since race is in automatically, like age and year
  c=data.frame(cancer=cancers)
  cs=merge(c,s)%>%arrange(cancer,sex)
  # csr=merge(cs,r)%>%arrange(cancer,sex,race) #comment to keep label of race
  options(warn=-1)
  pL=left_join(cs,p,by="sex")
  d=left_join(pL,m)
  options(warn=0)
  d[is.na(d$n),"n"]=0 #join left missings where zero's should be, so fix this
  # head(d)
  d=d%>%group_by(cancer)%>%filter(year>=startYrs[as.character(cancer[1])])
  # reshape2::dcast(d%>%group_by(cancer,year)%>%summarize(n=sum(n)),cancer~year) #check, should be same
  d%>%mutate(py=py/1e5,incid=n/py)
}

