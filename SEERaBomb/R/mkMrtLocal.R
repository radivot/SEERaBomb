mkMrtLocal=function(country="USA",mrtHome="~/data/mrt",
                    mrtSrc1="~/data/hmd_countries",
                    mrtSrc2="~/data/hmd_statistics/death_rates/Mx_1x1"
                    ){
  Year=NULL
  # country="USA";mrtHome="~/data/mrt"
  # mrtSrc1="~/data/hmd_countries"
  # mrtSrc2="~/data/hmd_statistics/death_rates/Mx_1x1"
  mrtSrc1=path.expand(mrtSrc1)
  mrtSrc2=path.expand(mrtSrc2)
  f1<-file.path(mrtSrc1,country,"STATS","Mx_1x1.txt")
  f2<-file.path(mrtSrc2,paste0(country,".Mx_1x1.txt"))
  
  if(file.exists(f1)) {f=f1; cat("Using  input file:",f,"\n")} else
    if(file.exists(f2)) {f=f2; cat("Using  input file:",f,"\n")} else stop("Cannot find local Human Mortality Data!") 
  X=read_table(f,skip=2,na=".")
  # X=X%>%filter(Year<2021)  # change this every year to sync with SEER
  X$Age[X$Age=="110+"]="110"
  X$Age=as.numeric(X$Age)
  # X$Male=as.numeric(X$Male)
  # X$Female=as.numeric(X$Female)
  F=X[,1:3]
  M=X[,c(1:2,4)]
  names(M)[3]="rate"
  names(F)[3]="rate"
  spreadit=function(M) {
    M=spread(M,key=Year,value="rate")
    M$Age=NULL
    M=as.matrix(M)
    row.names(M)=c(0:109,"110+")
    M
  }
  M=spreadit(M)
  F=spreadit(F)
  mrt=NULL
  mrt$Female=F
  mrt$Male=M
  
  mrtHome=path.expand(mrtHome)
  if(!dir.exists(mrtHome)) dir.create(mrtHome,recursive=T)
  save(mrt,file=f<-file.path(mrtHome,paste0("mrt",country,".RData")))
  cat("The dataframe mrt has been written to:",f,"\n")
}
