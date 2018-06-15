esd=function(d,srfF,srfM,brkst=c(0,2,5),brksy=NULL){ 
  esd2=function(d,srfF,srfM,frst,sec,brkst,brksy){ #event since diagnosis (esd)
    Eincid=age=cancer=cancer1=year=NULL
    Xf=reshape2::acast(srfF%>%filter(cancer==sec)%>%select(year,age,Eincid), age~year, value.var="Eincid")
    Xm=reshape2::acast(srfM%>%filter(cancer==sec)%>%select(year,age,Eincid), age~year, value.var="Eincid")
    extendX=function(X) { #extend incidence D matrices to be just like mrt matrices
      nrows=dim(X)[1]
      ncols=dim(X)[2]
      Bot=matrix(rep(X[nrows,],11),ncol=ncols,byrow=T)
      X=rbind(X,Bot)
      if (ncols<30) {  # i.e. 13 for MDS this year, extend left, else leave alone
        Left=matrix(rep(X[,1],28),nrow=111)
        X=cbind(Left,X)
      }
      colnames(X)=1973:2015
      rownames(X)=0:110
      rownames(X)[111]="110+"
      X/1e5
    }
    Xm=extendX(Xm)
    Xf=extendX(Xf)
    mrt=list(Female=Xf,Male=Xm)
    d=d%>%filter(cancer1==frst)
    d$cancer1=NULL
    d$surv=d$py
    d$py=NULL
    d$status=0
    d$status[d$cancer2==sec]=1
    d$cancer2=NULL
    # head(d)
    msd(d,mrt,brkst,brksy)
  }
  frstS=unique(d$cancer1)
  secS=setdiff(unique(d$cancer2),"none")
  D=NULL
  # debug(esd2)
  for (i in frstS) 
    for (j in secS)
      D=rbind(D,cbind(esd2(d,srfF,srfM,frst=i,sec=j,brkst,brksy),cancer1=i,cancer2=j))
  D
}
