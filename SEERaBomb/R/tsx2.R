tsx2=function(seerSet,brks=c(0,2,5), outDir="~/Results",txt=NULL) { # FL for file list
  surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=L2D=NULL 
  print(binS<-levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsx interval/row names 
  #   load(FL$fL)   # bring in L2D
  #   load(FL$fS)   # bring in seerSet
  #   bf=FL$f  # f holds the base filename
  #   seerSet
  ptm <- proc.time()
  L=with(seerSet, {
    #     canc=seerSet$canc
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
        L1=post1PYO(canc,brks,binIndx,Trt=R )
        #         print(D)
        Exp[[bin]]=getE2(L1$LPYM,D)
        Obs[[bin]]=L1$O
        mids=c(mids,L1$mid)
      } # loop on tsx bins
      L[[R]]$mids=mids
      L[[R]]$Obs=Obs
      L[[R]]$Exp=Exp
    } # loop on R
    L
  })
  seerSet$L=L
  seerSet$bfn=paste0(seerSet$bfn,paste0("b",paste(brks,collapse="_")),txt)
  fn=paste0(outDir,"/",seerSet$bfn,".RData")
  cat("Writing L to ",fn,"\n")
  save(L,file=fn)
  seerSet$fL=fn
  print(proc.time() - ptm)
  seerSet
}

