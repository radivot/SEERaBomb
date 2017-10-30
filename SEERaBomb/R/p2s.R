p2s=function(canc,firstS,secondS,yrcut=2010) {
  cancer=surv=yrdx=seqnum=NULL 
  # df names  were chosen thinking myeloid neoplasms (mn) are primary cancers and AMLs are secondary
  canc$yrmodx=round((canc$modx-0.5)/12+canc$yrdx,3) # Make yrmodx to improve resolution over yrdx: 2014.0=Jan 1, 2014
  (mn=canc%>%filter(cancer%in%firstS,seqnum<2, yrdx>=yrcut)) 
  mn$cancer=factor(mn$cancer,levels=firstS)
  aml=canc%>%filter(cancer%in%secondS,seqnum==2, yrdx>=yrcut)
  mrn=intersect(mn$casenum,aml$casenum)
  mn1=mn[mn$casenum%in%mrn,]
  aml2=aml[aml$casenum%in%mrn,]
  mn1$casenum=as.numeric(mn1$casenum)
  aml2$casenum=as.numeric(aml2$casenum)
  mn1=mn1[order(mn1$casenum),]
  aml2=aml2[order(aml2$casenum),]
  aml2$trt=mn1$trt
  aml2$yrdiff=aml2$yrmodx-mn1$yrmodx
  mn1$surv=aml2$yrdiff*12
  mn1$cancer2=aml2$cancer
  mnNoC2=subset(mn,!(mn$casenum%in%mrn))
  mnNoC2$c2occ=0
  mn1$c2occ=1
  mnNoC2$cancer2=NA
  d=rbind(mn1,mnNoC2)
  d=d%>%filter(surv<9999)
  d$status=0
  d$status[d$cancer2%in%secondS]=1
  d
}

