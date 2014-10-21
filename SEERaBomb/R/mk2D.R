mk2D<-function(seerSet, knots=5, write=FALSE, outDir="~/Results", txt=NULL) {
  if(!file.exists(outDir))  {   print(paste("Creating directory",outDir))
                                dir.create(outDir,recursive=TRUE)    }
#   require(dplyr)
#   require(mgcv) # Mixed GAM Computation Vehicle with GCV/AIC/REML smoothness estimation
  ptm <- proc.time()
  seerSet=with(seerSet, {
    L2D=vector(mode="list",length=length(picks)) 
    names(L2D)=picks
    L2Dp=L2D; D=NULL; 
    knotsIn=knots
    for (i in picks) {
      knots=knotsIn
      if (i=="ALL" & ageStart<15) knots=20   # make some knot numbers conditional, like this
      if (i=="otherCIS" & knots<10) knots=10   # 5 doesn't cut it for this, 10 does
      if (i=="liver" & knots<10) knots=10   # same here, to capture the cohort effect ripple
      if (i=="APL" & knots>5) knots=5   # here too many knots dive down too much into early calendary years of too few cases
      #the next if else deals with MDS and CMML starting only later. Not sure what other cancers are like this.
      if (i=="MDS")  {d=canc%>%filter(cancer%in%i,year>2000)
                      ps=popsa%>%filter(year>2000)
      } else 
        if (i=="CMML")  {d=canc%>%filter(cancer%in%i,year>1985) 
                         ps=popsa%>%filter(year>1985)
        } else  {
          d=canc%>%filter(cancer%in%i)
          ps=popsa
        }
      d=d%>%group_by(year,age)%>%summarise(cases=n())
      ps=ps%>%group_by(year,age)%>%summarise(py=sum(py)) 
      X=left_join(ps,d)%>%mutate(incid=1e5*cases/py)
      X[is.na(X)]=0
      cat(knots," knots, working on cancer ",i,":\n")
      L2D[[i]]=gam(cases ~ s(age)+s(year)+ti(age,year,k=knots)+offset(log(py)),
                   family=poisson(),data=X,method="REML") 
      L2Dp[[i]]=cbind(cancer=i,X,Ecases=exp(predict(L2D[[i]])),Eincid=1e5*exp(predict(L2D[[i]]))/X$py  )
      D=rbind(D,L2Dp[[i]])
    } # i loop on cancers in picks
    bfn<-paste0(substr(race,1,1),toupper(substr(sex,1,1)),"s",ageStart,"e",ageEnd,txt) #base of file names
    seerSet$D=tbl_df(D)
    seerSet$bfn=bfn
    if (write) {
      print("writing L2D file ...")
      f<-paste0(outDir,"/",bfn) 
      fL<-paste0(f,"L.RData");    #fD<-paste0(f,"D.RData");   fS<-paste0(f,"S.RData") 
      print(fL);save(L2D,file=fL);  # print(fD); save(D,file=fD);   print(fS);save(seerSet,file=fS)
      seerSet$fL=fL
    }
    seerSet
  }) # end with
  print(proc.time() - ptm) 
  seerSet # return extended seerSet, now including D and file base name, and the L2D file name, if written
} #end func
