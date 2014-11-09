plot2D<-function(seerSet, outDir="~/Results/plots") {
  with(seerSet, {
    #     if(!file.exists(fD)) stop(paste0(fD,"does not exist!")) 
    #     load(fD) # brings in dataframe D
    if(nchar(bfn)==0) {bfn="tmp"
                       warning("no base file name, so files will go to subfolder tmp")}
    outDir=file.path(outDir,bfn)
    if(!file.exists(outDir))  {   print(paste("Creating directory",outDir))
                                  dir.create(outDir,recursive=TRUE)    }
    #     require(rgl)
    #     require(dplyr)
    picks=levels(D$cancer)
    #     head(D,2)
    print("If rgl is being loaded for the first time in this R session, make the plot bigger and get a nice angle on it before hitting returns at the R command prompt.")
    for (i in picks) {
      pd=D%>%filter(cancer==i)%>%select(year,age,incid,Eincid)
      #     head(pd)
      #     sapply(pd,class)
      (nyrs=length(yrs<-unique(pd$year)))
      (length(ages<-unique(pd$age))*nyrs)
      pd$incid[pd$incid==0]=0.001
      with(pd,plot3d(age,year,log10(incid),xlab="",ylab="",zlab="")) #,sub=paste0(j," ",i,"s")))
      with(pd,surface3d(x=ages,y=yrs,z=matrix(log10(Eincid),ncol=nyrs),alpha=0.8,col="red"))
      clear3d(type="lights")
      light3d(theta = -90, phi = 75) 
      cat(i,"  ...  hit return at command prompt to save and move on:\n")
      readline() # way to pause until input
      rgl.snapshot(filename=paste0(outDir,"/",i,".png"),fmt="png", top=TRUE)
    } #i loop over cancers
    
  }) # end with 
    
}  # plot2D function
