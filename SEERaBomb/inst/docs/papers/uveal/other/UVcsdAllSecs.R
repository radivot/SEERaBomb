graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
# load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
# load("~/data/SEER/mrgd/popsae.RData")
# (codes=paste0("C",692:694))
# canc$cancer=as.character(canc$cancer)
# canc[(canc$primsite%in%codes)&(canc$histo3%in%8720:8790),"cancer"]="uveal"  
# pf=seerSet(canc,popsae,Sex="Female");pm=seerSet(canc,popsae,Sex="Male") 
# pf=mk2D(pf);pm=mk2D(pm) # all secs (takes time!!)
# mybrks=c(0,1,2,3,5,10)
# pf=csd(pf,brkst=mybrks)  #  (takes time!!)
# pm=csd(pm,brkst=mybrks)  #  (takes time!!)
# # (lab=paste0("b",paste(mybrks,collapse="_")))
# # mkExcelCsd(pf,lab,outDir="uveal/outs",outName="csdFall")
# # mkExcelCsd(pm,lab,outDir="uveal/outs",outName="csdMall")
# # save(pm,pf,file="uveal/data/pmpf.RData")  # takes a while!!!
load(file="uveal/data/pmpf.RData")  
DF=bind_rows(pf$DF,pm$DF)
DF=rad_noRad(DF)
DF=foldDF(DF)
UM=DF%>%filter(cancer1=="uveal")
UM%>%group_by(trt)%>%summarize(O=sum(O))  # ~1100 second cancers after uveal
(CML=UM%>%filter(cancer2=="CML"))
CML%>%group_by(trt)%>%summarize(O=sum(O))  # none of which are CMLs ... skip idea of uveal rad causing cancer
UM%>%filter(rrL>1.1,!cancer2%in%c("eye","melanoma","uveal","otherMalig","skinCIS"))%>%print(n=150)
# liver likely mets of uveal, renal and thyroid likely overdiagnosis due to CT scans
# MPN and CLL are interesting and perhaps related to immunological aspects of uveal control
### lung cancer is rad, are isotopes moving to the lung, or is this also due to CT scan detecting them early.


