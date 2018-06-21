mkLT<-function(mrtHome="~/data/usMort",input="mrt.RData",output="ltb.RData"){
  # mrtHome="~/data/usMort";input="mrt.RData";output="ltb.RData"
  mrt=NULL
  mrtHome=path.expand(mrtHome)
  if(!dir.exists(mrtHome)) stop("The directory mrtHome does not exist. Use mkMrt to create it and place mortality data in it.")
  inp=file.path(mrtHome,input) 
  out=file.path(mrtHome,output) 
  outXL=file.path(mrtHome,"LT.xls") 
  load(inp)#loads US mortality data
  ltb=NULL
  for (sex in c("Male","Female")){
    # (M=mrt$Female)
    (M=mrt[[sex]])
    (LT=M) # replace rates with Esurvs
    aN=dim(LT)[1]
    yN=dim(LT)[2]
    #need to slap on copies of final column to right for future of the young 
    (Mfill=matrix(M[,yN],nrow=111,ncol=111))
    (Mbig=cbind(M,Mfill))
    for (i in 1:aN) 
      for (j in 1:yN) 
      {
        Palive=1
        # cat("i=",i,"j=",j,"k=",k,"P=",Palive,"\n")
        for (k in 0:(aN-i)) 
        {
          Palive=Palive*(1-Mbig[i+k,j+k])
          if (Palive<0.5) break
        }
        # k=k+(0.5-Palive)
        LT[i,j]=k#-1/log(Palive)
      }
    ltb[[sex]]=as.data.frame(LT)
  }
  save(ltb,file=out)
  WriteXLS(ltb,outXL,row.names = T,FreezeRow = 1, FreezeCol = 1)
  cat("The lifetable list ltb has been written to the files ",out," and ",outXL,"\n")
}
