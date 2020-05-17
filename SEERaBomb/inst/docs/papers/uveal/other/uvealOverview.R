graphics.off();rm(list=ls())#clear plots and environment
library(WriteXLS) 
library(dplyr) 
library(scales)    
cf=function (x) comma_format(width=17,justify="right")(x)

load("~/data/SEER/mrgd/cancPrim.RData")
load("~/data/SEER/mrgd/popsae.RData")
head(popsae)
PY=(popsae%>%group_by(year)%>%summarize(PY=sum(py)))[[2]]

(codes=paste0("C",690:699))
d=canc%>%filter(primsite%in%codes,histo3%in%8720:8790) 
d$cancer=as.character(d$cancer)
# C69.3 (choroid), C69.4 (ciliary body and iris), and C69.2 (retina). 
d$cancer[d$primsite=="C690"]="C690-Conjunctiva"
d$cancer[d$primsite=="C691"]="C691-Cornea"
d$cancer[d$primsite=="C692"]="C692-Retinal"
d$cancer[d$primsite=="C693"]="C693-Choroid"
d$cancer[d$primsite=="C694"]="C694-Ciliary"
d$cancer[d$primsite=="C695"]="C695-Lacrimal"
d$cancer[d$primsite=="C696"]="C696-Orbital"
d$cancer[d$primsite=="C698"]="C698-Overlap"
d$cancer[d$primsite=="C699"]="C699-NOS"

unlink(f<-"uveal/outs/uvealOverview.xlsx") 
yrca=table(d$yrdx,d$cancer)
yrTot=apply(yrca,1,sum)
Total=apply(yrca,2,sum)
gtot=sum(Total)
D1=data.frame(year=c(1975:2016,NA),as.data.frame(rbind(yrca,Total)),total=c(yrTot,gtot),PY=cf(c(PY,sum(PY))))

total=table(d$cancer)
h3ca=table(d$histo3,d$cancer)
D2=as.data.frame(cbind(hist03=c(as.numeric(row.names(h3ca)),NA),rbind(h3ca,total)))

total=table(d$histo3)
yrh3=table(d$yrdx,d$histo3)
D3=as.data.frame(cbind(year=c(1975:2016,NA),rbind(yrh3,total)))
WriteXLS(list(yearXcancer=D1,histo3Xcancer=D2,yearXhisto3=D3),ExcelFileName=f,FreezeRow=1,FreezeCol=1,AdjWidth=T)


