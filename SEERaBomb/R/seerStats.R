seerStats<-function(canc,popsa) {
  age=agedx=db=py=reg=cancer=over99=total=NULL 
  #   cf=function (x) comma_format()(x)
  #seerStats(canc,popsae)
  L=NULL
  L$P=popsa%>%group_by(db,reg)%>%summarize(PY=round(sum(py)/1e6,1),age=weighted.mean(age,py))
  D=canc%>%group_by(cancer,db)%>%summarize(n=n())
  A=canc%>%filter(agedx>99.5)%>%group_by(cancer)%>%summarize(n=n())
   C=dcast(D,cancer~db,value.var="n",fun.aggregate = sum,margins=c("db"))
#   C=dcast(D,cancer~db,value.var="n",fun.aggregate = sum,margins=TRUE)
  O=dcast(A,cancer~.,value.var="n")
  d=left_join(C,O,by="cancer")
  names(d)[5:6]=c("total","over99")
  d=d%>%mutate(under100=total-over99)
  d[is.na(d)]=0
  d$cancer=as.character(d$cancer)
  d[dim(d)[1]+1,]=data.frame("total",t(sapply(d[-1],sum)))
  d[dim(d)[1],1]="total"
  L$d=d
  cat(paste("Cases per SEER database through",max(popsa$year),"(sexes and races pooled).\n"))
  print(d) 
  print(L$P) 
  cat(paste("Penultimate column: PY in millions are through",max(popsa$year),"and sexes and races pooled.\n"))
  cat(paste("Last column: average ages are PY-weighted.\n"))
  invisible(L)
} 
