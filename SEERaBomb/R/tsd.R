tsd=function(seerSet,brks=c(0,2,5),trts=NULL,PYM=FALSE,firstS="all"){ 
#   surv=yrdx=modx=db=casenum=radiatn=cancer=trt=yrbrth=agedx=L2D=NULL
  print(binS<-levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsd interval/row names 
  ptm <- proc.time()
  if(is.null(seerSet$L)) seerSet$L=list() # set list L to a blank list if it was never filled for this seerSet
  SL=with(seerSet, {  # subL (List within L), i.e. L is a list of time series sublists (SL). 
#     yearEnd=max(popsa$year)
    SL=list() # initiate the SL that tsd() adds to L
    if (firstS[1]=="all") firstS=cancerS
    SL$firstS=firstS
    if (is.null(trts)) trts=levels(canc$trt)
    SL$trtS=trts
    print(trts)
    for (R in trts) 
    { 
      #     R="rad"
       print(R)
      mids=NULL
      Obs=vector(mode="list",length=0)
      Exp=vector(mode="list",length=0)
      PyM=vector(mode="list",length=0)
      for (bin in binS) 
      {
        #       (bin=binS[1])
#         print(bin)
        binIndx=getBinInfo(bin,binS)["index"]
        L1=post1PYO(canc,brks,binIndx,Trt=R,yearEnd,firstS,secondS=secondS)
        Exp[[bin]]=getE(L1$LPYM,D,ageStart,ageEnd,yearEnd,firstS,secondS)
#          Obs[[bin]]=L1$O
        Obs[[bin]]=L1$O[firstS,secondS,drop=FALSE]
#         rws=rownames(L1$O)
#         cols=colnames(L1$O)
#         Obs[[bin]]=L1$O[rws%in%firstS,cols%in%secondS,drop=FALSE]
        PyM[[bin]]=L1$PY
        mids=c(mids,L1$binMidPnt)
      } # loop on tsx bins
      SL[[R]]$mids=mids
      SL[[R]]$Obs=Obs
      SL[[R]]$Exp=Exp
      if (PYM) SL[[R]]$PyM=PyM
    } # loop on R
     SL
   })
  tsdn=paste0("b",paste(brks,collapse="_"))
#   seerSet$bfn=paste0(seerSet$bfn,paste0("b",paste(brks,collapse="_")),txt)
  seerSet$L[[tsdn]]=SL
#   fn=paste0(outDir,"/",seerSet$bfn,".RData")
#   cat("Writing L to ",fn,"\n")
#   save(L,file=fn)
#   seerSet$fL=fn
  print(proc.time() - ptm)
  seerSet
}

