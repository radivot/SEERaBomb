msd=function(canc,mrt,brkst=c(0,2,5),brksy=NULL){ #mortality since diagnosis (msd)
  yrdx=Years=start=yearInt=stop=NULL
  msd2=function(canc,mrt,brks){ #old version
    surv=sex=O=E=NULL
    # yearEnd=ceiling(max(canc$yrdx+canc$surv))
    yearEnd=max(as.numeric(colnames(mrt$Female)))
    canc=canc%>%filter(surv<200) # restrict to known survival times
    dm=canc%>%filter(sex=="Male")%>%select(-sex)
    df=canc%>%filter(sex=="Female")%>%select(-sex)
    d=list(Male=dm,Female=df)
    mrtF=mrt$Female
    nages=dim(mrt$Female)[1]
    nfill=126-nages
    # mrtF=rbind(mrtF,sapply(mrtF[111,],function(x) rep(x,15)))
    mrtF=rbind(mrtF,sapply(mrtF[nages,],function(x) rep(x,nfill)))
    rownames(mrtF)=0:125
    mrtM=mrt$Male
    mrtM=rbind(mrtM,sapply(mrtM[nages,],function(x) rep(x,nfill)))
    rownames(mrtM)=0:125
    mrtM=mrtM[,as.character(1975:yearEnd)]
    mrtF=mrtF[,as.character(1975:yearEnd)]
    mrt=list(Male=mrtM,Female=mrtF)
    pts=c(Male=dim(dm)[1],Female=dim(df)[1],total=dim(dm)[1]+dim(df)[1])
    events=c(Male=sum(dm$status),Female=sum(df$status),Total=sum(dm$status)+sum(df$status))
    Sexes=c("Male","Female")[pts[1:2]>0]
    print(Sexes)
    print(binS<-levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsd interval/row names 
    DD=NULL
    for (S in Sexes) 
    { 
      print(S)
      mids=vector(mode="numeric",length=0)
      Obs=vector(mode="numeric",length=0)
      Exp=vector(mode="numeric",length=0)
      for (bin in binS) 
      {
        binIndx=getBinInfo(bin,binS)["index"]
        L1=post1PYOm(d[[S]],brks,binIndx,yearEnd)
        Exp[bin]=sum(L1$PYM*mrt[[S]]) 
        Obs[bin]=L1$O
        mids[bin]=L1$binMidPnt
      } # loop on tsx bins
      D=data.frame(int=factor(names(mids)),t=mids,O=Obs,E=Exp)
      D=D%>%mutate(RR=O/E,
                   rrL=qchisq(.025,2*O)/(2*E),
                   rrU=qchisq(.975,2*O+2)/(2*E),sex=S)
      DD=rbind(DD,D)
    } # loop on S (sexes)
    DD
  }
  if (!is.null(brksy)) {
    canc=canc%>%mutate(Years=cut(yrdx,breaks=brksy,dig.lab=4,include.lowest=T))
    L=split(canc,canc$Years)
    LO=lapply(L,function (x) msd2(x,mrt,brkst))
    # for (i in names(LO)) LO[[i]]$Years=i
    for (i in names(LO)) LO[[i]]$yearInt=i
    D=bind_rows(LO)
    # D=D%>%tidyr::separate(yearInt,c("start","stop"),sep=",",remove=F)
    D=D%>%tidyr::separate(yearInt,c("start","stop"),sep=",")
    # D$start
    strPl1=stringr::str_detect(D$start, "^\\(")
    D$start=stringr::str_remove(D$start, "[\\[\\(]")
    D$start[strPl1]=as.numeric(D$start[strPl1])+1
    D$stop=stringr::str_remove(D$stop, "[\\]\\)]")
    D=D%>%tidyr::unite(Years,start:stop,sep="-")
    D$Years=as_factor(D$Years)
  } else D=msd2(canc,mrt,brkst)
  as_tibble(D)  
}
