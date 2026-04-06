mkG6<-function(seerHome="~/data/seer25",
               inFile="seerMrt.RData",
               outFile="G6.RData"){
  # Use mkSEERmrt() to make inFile 
  # seerHome="~/data/seer25"
  # inFile="seerMrt.RData"
  # outFile="G6.RData"
  # require(dplyr)
  # require(forcats)
  # require(mgcv)
  # require(purrr)
  # require(rgl)
  # options(rgl.useNULL = TRUE)
  # options(rgl.printRglwidget = TRUE)
  d=COD=num=denom=year=sex=Ages=age=rate=W=NULL
  load(file.path(seerHome,inFile)) # get d with dims 219,520 × 7
  (d=d%>%select(COD,year,sex,age,denom,num)%>%arrange(COD,year,sex,age))
  ddx=d%>%group_by(COD)%>%summarise(num=sum(num),.groups="drop")
  # warning: mortality data COD defs below need to be synced up with reg data COD defs in mkCML()
  L=NULL
  tits=NULL
  dl=d%>%filter(COD%in%c(70))
  dl=dl%>%mutate(COD="LC")
  (dl=dl%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  (L[["LC"]]=dl)
  tits["LC"]="Leukemic Cause"

  dc=d|>filter(COD%in%c(1,84))|>mutate(COD="CA") # 84 = benign included, 
  (dc=dc%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  (dc=dc%>%mutate(num=num-dl$num))  #and LC removed
  (L[["CA"]]=dc)
  tits["CA"]="Cancer"
  
  di=d%>%filter(COD%in%c(85:89,98)) #TB, syph, hiv, septicemia, parasites 98=flu
  di=di%>%mutate(COD="IN") #INfections
  (L[["IN"]]=di%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["IN"]="Infection"
  
  dv=d%>%filter(COD%in%c(92:97))%>%mutate(COD="CV") 
  #92=heart disease, 93=#Hypertension without Heart Disease, 94=Cerebrovascular Diseases 
  # 95 = Atherosclerosis; 96 = Aortic Aneurysm 97 = OD 
  (L[["CV"]]=dv%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["CV"]="Cardio and Vascular"
  
  dk=d%>%filter(COD%in%c(90,102))%>%mutate(COD="DK")  #90=DM,  # 102 = kidney disease
  (L[["DK"]]=dk%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["DK"]="DM & Kidney Disease"
  
  dash=d%>%filter(COD%in%c(107:109))%>%mutate(COD="ASH") #accidents, suicides and homocides
  (L[["ASH"]]=dash%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["ASH"]="Accidents, Suicides and Homocides"
  
  do=d%>%filter(COD%in%c(91,99:101,103:106,111))%>%mutate(COD="YOC") #yet other causes
  # 91=Alzheimers,99 = COPD, 100= stomach ulcers, 103:105 = Complications of birth, congenital, perinatal conditions 
  # 101= liver disease
  #add perinatal and ulcers to ill-defined conditions and other causes of death 
  # do=d%>%filter(COD%in%c(106,111)) #ill-defined conditions and other causes of death 
  (L[["YOC"]]=do%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["YOC"]="Yet Other Causes"
  (nms=names(L))
  G=vector(mode="list",length=length(L))
  names(G)<-nms
  for (i in nms) {
    # i="LC"
    print(i) 
    D=L[[i]]
    # (D=D%>%filter(age<90,age>40))
    # (D=D%>%filter(age<90,age>25))
    (D=D%>%filter(age>20))
    D$sex=as_factor(D$sex)
    (Df=D%>%filter(year<2020)) # data for fitting
    print(summary(G[[i]]<-gam(num ~ sex+s(age,year,by=sex)+ti(age,year)+offset(log(denom)),family=poisson(),data=Df)))
    # D$E=exp(predict(G[[i]],D))
    # head(D<-D%>%mutate(Eincid=E/denom))
    # head(D<-D%>%mutate(incid=num/denom))
    # head(D<-D%>%mutate(incid=if_else(incid==0,0.0001,incid)))
    # with(D,plot3d(year,age,log10(incid),col=ifelse(sex=="Female","red","blue"),
    #               xlab="",ylab="Age",zlab="log10(M)",alpha=1,size=4))
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
    # nD$E=exp(predict(G[[i]],nD))
    # M=reshape2::acast(nD%>%filter(sex=="Female")%>%select(year,age,E), year~age, value.var="E")
    # surface3d(Years,Ages,log10(M),col="red",alpha=0.5) # M for Matrix
    # M=reshape2::acast(nD%>%filter(sex=="Male")%>%select(year,age,E), year~age, value.var="E")
    # surface3d(Years,Ages,log10(M),col="blue",alpha=0.5)
    # light3d(theta = 0, phi = 75) 
    # bgplot3d({
    #   plot.new()
    #   title(main = paste0(i), line = 0,cex.main=1)
    # })
    # readline(prompt=paste("Plotting",i,"Press [enter] to continue, Ctrl-C to exit"))#uncomment for snapshots
  }
  save(G,L,tits,file=file.path(seerHome,outFile)) #0.03 secs  33,541 CML cases
}
 
