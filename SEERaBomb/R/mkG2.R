mkG2<-function(seerHome="~/data/seer25",
               inFile="seerMrt.RData",
               outFiles=c("Glc.RData","Goc.RData","Gac.RData")){

  # Use mkSEERmrt() to make inFile and note in code therein that
  # 66="    Lymphoma"
  # 67="      Hodgkin Lymphoma"
  # 68="      Non-Hodgkin Lymphoma"
  # 69="    Myeloma"
  # 70="    Leukemia"   ## in cod111.dic
  
  # seerHome="~/data/seer25"
  # inFile="seerMrt.RData"
  # outFiles=c("Glc.RData","Goc.RData","Gac.RData")
  # require(dplyr)
  # require(forcats)
  # require(mgcv)
  # require(rgl)
  # require(purrr)
  d=COD=num=denom=year=sex=Ages=age=rate=W=NULL
  
  # b=readr::read_csv(file.path(seerHome,inDir, inFiles[1]), col_names=vars,skip=1) 
  # p=readr::read_csv(file.path(seerHome,inDir, inFiles[2]), col_names=popVars,skip=1) 
  # b=b|>dplyr::filter(COD==0|COD==3)
  # d=dplyr::left_join(b,p)
  # (d=d|>mutate(rate=num/denom,year=year+1975,sex=ifelse(sex==0,"Male","Female")))
  # tail(d)
  # #load Ages, a list of two sexes of age-year matrices of PY-weighted 5-year bin age-group midpoints 
  # load(meanAgeFile) # made in mrtAges.R
  # getAge=function(age,year,sex) Ages[[sex]][[as.character(age),as.character(year)]]
  # (d=d%>%mutate(age=pmap_dbl(list(age,year,sex),getAge))) #update ages to set up Poisson regressions
  load(file.path(seerHome,inFile)) # get d with dims 219,520 × 7
  (da=d%>%filter(COD==0)%>%mutate(COD="AC")%>%select(-rate))
  # (dl=d%>%filter(COD>0)%>%mutate(COD="LC")%>%select(-rate))
  (dl=d%>%filter(COD==70)%>%mutate(COD="LC")%>%select(-rate))
  (dl=dl%>%group_by(COD,sex,age,year,denom)%>%summarize(num=sum(num),.groups="drop"))
  sum(dl$num) # 994475 checks with total US leukemia deaths in 1969-2023
  (da=da[,names(dl)]%>%arrange(sex,age,year))
  (do=da%>%mutate(COD="OC",num=num-dl$num))
  
  D=dl
  (D=D%>%filter(age>20))
  D$sex=as_factor(D$sex)
  # (Df=D%>%filter(year<2020)) # data for fitting skips 2020
  Df=D  #leave it in so covid deaths don't go to TKI
  summary(Glc<-gam(num ~ sex+s(age,year,by=sex)+ti(age,year)+offset(log(denom)),family=poisson(),data=Df)) 
  # D$E=exp(predict(Glc,D))       # commented blocks are just for checking mortality surfaces
  # head(D<-D%>%mutate(Eincid=E/denom))
  # head(D<-D%>%mutate(incid=num/denom))
  # head(D<-D%>%mutate(incid=if_else(incid==0,0.0001,incid)))
  # with(D,plot3d(year,age,log10(incid),col=ifelse(sex=="Female","red","blue"),xlab="Year",ylab="Age",zlab="log10(M)",alpha=1,size=4))
  # 
  # (Ages=seq(min(D$age),max(D$age)))
  # (Years=seq(min(D$year),max(D$year)))
  # head(nD<-expand.grid(Ages,Years))
  # names(nD)<-c("age","year")
  # nD$denom=1
  # nDf=nD%>%mutate(sex="Female")
  # nDm=nD%>%mutate(sex="Male")
  # nD=bind_rows(nDf,nDm)
  # head(nD)
  # dim(nD)  # = 99*35
  # nD$E=exp(predict(Glc,nD))
  # M=reshape2::acast(nD%>%filter(sex=="Female")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="red",alpha=0.5) # M for Matrix
  # M=reshape2::acast(nD%>%filter(sex=="Male")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="blue",alpha=0.5)
  # clear3d(type="lights")
  # light3d(theta = 0, phi = 70) #expand X window and take snapshot into powerpoint (from there export as pdf)
  # save(Glc,file="~/data/seer25/Glc.RData")
  save(Glc,file=file.path(seerHome,outFiles[1])) #0.03 secs  33,541 CML cases
  
  
  ## other cause (OC) mortality
  D=do
  (D=D%>%filter(age>20))
  D$sex=as_factor(D$sex)
  # (Df=D%>%filter(year<2020)) # data for fitting skips 2020
  Df=D  #leave it in so covid deaths don't go to TKI
  summary(Goc<-gam(num ~ sex+s(age,year,by=sex)+ti(age,year)+offset(log(denom)),family=poisson(),data=Df))  
  # D$E=exp(predict(Goc,D))
  # head(D<-D%>%mutate(Eincid=E/denom))
  # head(D<-D%>%mutate(incid=num/denom))
  # head(D<-D%>%mutate(incid=if_else(incid==0,0.0001,incid)))
  # with(D,plot3d(year,age,log10(incid),col=ifelse(sex=="Female","red","blue"),xlab="Year",ylab="Age",zlab="log10(M)",alpha=1,size=4))
  # 
  # (Ages=seq(min(D$age),max(D$age)))
  # (Years=seq(min(D$year),max(D$year)))
  # head(nD<-expand.grid(Ages,Years))
  # names(nD)<-c("age","year")
  # nD$denom=1
  # nDf=nD%>%mutate(sex="Female")
  # nDm=nD%>%mutate(sex="Male")
  # nD=bind_rows(nDf,nDm)
  # head(nD)
  # dim(nD)  # = 99*35
  # nD$E=exp(predict(Goc,nD))
  # M=reshape2::acast(nD%>%filter(sex=="Female")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="red",alpha=0.5) # M for Matrix
  # M=reshape2::acast(nD%>%filter(sex=="Male")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="blue",alpha=0.5)
  # clear3d(type="lights")
  # light3d(theta = 0, phi = 70) 
  save(Goc,file=file.path(seerHome,outFiles[2])) #0.03 secs  33,541 CML cases

  ## all cause (AC) mortality
  D=da
  (D=D%>%filter(age>20))
  D$sex=as_factor(D$sex)
  # (Df=D%>%filter(year<2020)) # data for fitting skips 2020
  Df=D  #leave it in so covid deaths don't go to TKI
  summary(Gac<-gam(num ~ sex+s(age,year,by=sex)+ti(age,year)+offset(log(denom)),family=poisson(),data=Df))  
  # D$E=exp(predict(Gac,D))
  # head(D<-D%>%mutate(Eincid=E/denom))
  # head(D<-D%>%mutate(incid=num/denom))
  # head(D<-D%>%mutate(incid=if_else(incid==0,0.0001,incid)))
  # with(D,plot3d(year,age,log10(incid),col=ifelse(sex=="Female","red","blue"),xlab="Year",ylab="Age",zlab="log10(M)",alpha=1,size=4))
  # 
  # (Ages=seq(min(D$age),max(D$age)))
  # (Years=seq(min(D$year),max(D$year)))
  # head(nD<-expand.grid(Ages,Years))
  # names(nD)<-c("age","year")
  # nD$denom=1
  # nDf=nD%>%mutate(sex="Female")
  # nDm=nD%>%mutate(sex="Male")
  # nD=bind_rows(nDf,nDm)
  # head(nD)
  # dim(nD)  # = 99*35
  # nD$E=exp(predict(Gac,nD))
  # M=reshape2::acast(nD%>%filter(sex=="Female")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="red",alpha=0.5) # M for Matrix
  # M=reshape2::acast(nD%>%filter(sex=="Male")%>%select(year,age,E), year~age, value.var="E")
  # surface3d(Years,Ages,log10(M),col="blue",alpha=0.5)
  # save(Gac,file="~/data/seer25/Gac.RData")
  save(Gac,file=file.path(seerHome,outFiles[3])) #0.03 secs  33,541 CML cases
  
}
 
