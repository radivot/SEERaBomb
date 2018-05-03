incidAbomb=function(d) {
  py=O=.=NULL
  # load("~/data/abomb/abomb.RData")
  # head(heme,2)
  # library(tidyverse)
  # # (d=heme%>%select(ageG:DG,py,AML=AMLtot,ALL,CML))
  # (d=heme%>%select(ageG:DG,D,py,AML=AMLtot,ALL,CML))
  # d=d%>%group_by(ageG,DG)

  nms=names(d)
  if (!"DG"%in%nms) stop("Dose group column DG must be in d.")
  if (!"py"%in%nms) stop("Person-years column py must be in d.")
  end1=which(nms=="DG")
  start3=which(nms=="py")
  n=length(nms)
  hd=nms[1:end1]
  sums=nms[start3:n]
  cancers=sums[-1]
  if ("upy"%in%nms) cancers=cancers[-1]
  if ("subjects"%in%nms) cancers=cancers[-1]
  (mnsBlkNotEmpty=start3>(end1+1))
  if (mnsBlkNotEmpty) means=nms[(end1+1):(start3-1)]
  (ds=d%>%summarize_at(vars(sums),funs(sum)))
  if (mnsBlkNotEmpty) (dm=d%>%summarize_at(vars(means),funs(weighted.mean(.,w=py))))
  if (mnsBlkNotEmpty) D=left_join(dm,ds) else #like bind_cols() but removes extra col copies 
    D=ds
  options(warn=-1) #warning about column labels expected in next line
  D=D%>%tidyr::gather(value="O",key="cancer",cancers)#O=Observed
  options(warn=0)
  D$cancer=factor(D$cancer,levels=cancers)#set order
  tibble::as_tibble(D%>%mutate(py=py/1e5,I=O/py,LL=qpois(0.025,O)/py,UL=qpois(0.975,O)/py))
}
