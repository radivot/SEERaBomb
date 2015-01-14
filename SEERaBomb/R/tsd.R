tsd=function(seerSet,brks=c(0,2,5),trts=NULL){ #, outDir="~/Results",txt=NULL) { # FL for file list
  surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=L2D=NULL 
  print(binS<-levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsd interval/row names 
  ptm <- proc.time()
  L=with(seerSet, {
    if (is.null(trts)) trts=levels(canc$trt)
    seerSet$trts=trts
    yearEnd=max(popsa$year)
    print(trts)
    L=list()
    for (R in trts) 
    { 
      #     R="rad"
      print(R)
      mids=NULL
      Obs=NULL
      Exp=NULL
      PyM=NULL
      for (bin in binS) 
      {
        #       (bin=binS[1])
        print(bin)
        binIndx=getBinInfo(bin,binS)["index"]
        L1=post1PYO(canc,brks,binIndx,Trt=R )
        #         print(D)
        Exp[[bin]]=getE(L1$LPYM,D,ageStart,ageEnd,yearEnd,cancerS,picks)
        Obs[[bin]]=L1$O[cancerS,picks]
        PyM[[bin]]=L1$PY
        mids=c(mids,L1$binMidPnt)
      } # loop on tsx bins
      L[[R]]$mids=mids
      L[[R]]$Obs=Obs
      L[[R]]$Exp=Exp
      L[[R]]$PyM=PyM
    } # loop on R
    L
  })
  tsdn=paste0("b",paste(brks,collapse="_"))
#   seerSet$bfn=paste0(seerSet$bfn,paste0("b",paste(brks,collapse="_")),txt)
  if(is.null(seerSet$L)) seerSet$L=list() # set to a blank list if it was never filled
  seerSet$L[[tsdn]]=L
#   fn=paste0(outDir,"/",seerSet$bfn,".RData")
#   cat("Writing L to ",fn,"\n")
#   save(L,file=fn)
#   seerSet$fL=fn
  print(proc.time() - ptm)
  seerSet
}

