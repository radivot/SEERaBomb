tsx=function(seerSet,L2D,brks=c(0,2,5),bfn="", outDir="~/Results") { # FL for file list
  surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=L2D=NULL 
  print(binS<-levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsx interval/row names 
#   load(FL$fL)   # bring in L2D
#   load(FL$fS)   # bring in seerSet
#   bf=FL$f  # f holds the base filename
#   seerSet
  canc=seerSet$canc  
  ptm <- proc.time()
  trts=levels(canc$trt)
  print(trts)
  L=list()
  for (R in trts) 
  { 
    #     R="rad"
    print(R)
    mids=NULL
    Obs=NULL
    Exp=NULL
    for (bin in binS) 
    {
#       (bin=binS[1])
      print(bin)
      binIndx=getBinInfo(bin,binS)["index"]
      L=post1PYO(canc,brks,binIndx,Trt=R )
      Exp[[bin]]=getE(L$LPYM,L2D)
      Obs[[bin]]=L$O
      mids=c(mids,L$mid)
    } # loop on tsx bins
    L[[R]]$mids=mids
    L[[R]]$Obs=Obs
    L[[R]]$Exp=Exp
  } # loop on R
  fn=paste0(outDir,"/",bfn,"b",paste(brks,collapse="_"),".RData")
  cat("Writing L to ",fn,"\n")
  save(L,file=fn)
  print(proc.time() - ptm)
  L
}

