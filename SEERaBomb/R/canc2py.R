canc2py=function(canc,firstS,secondS) {
  COD=agedx=age=age1=agec=agedx=cancer=cancer1=cancer2=casenum=modx=NULL
  py=seqnum=sex=surv=trt=trt1=year=yrbrth=yrdiffn=yrdx=yrdx1=yrdx2=NULL
  # library(SEERaBomb)
  # load("~/data/SEER/mrgd/cancDef.RData") 
  # firstS=c("NHL","HL","MM")
  # secondS=c("AML","MDS")
  canc=canc%>%filter(cancer%in%union(firstS,secondS))
  canc=canc%>%mutate(year=yrdx) 
  canc=canc%>%mutate(yrdx=round(yrdx+(modx-0.5)/12,3))    #modx=1=January 
  canc=canc%>%mutate(surv=round((surv+0.5)/12,3))%>%  
    mutate(age=agedx+0.5) #convert ages at diagnosis to best guesses
  canc=canc%>%select(casenum,yrbrth,sex,age,seqnum,year,yrdx,surv,COD,trt,cancer) 
  D2=canc%>%filter(seqnum==2,cancer%in%secondS) # D2 holds second primaries
  D2$cancer=factor(D2$cancer,levels=secondS) # get rid of levels not in secondS
  D0=canc%>%filter(seqnum==0,surv<200,cancer%in%firstS)
  D0$cancer=factor(D0$cancer,levels=firstS) # get rid of levels not in firstS. 
  # head(D0,2)
  D1=canc%>%filter(seqnum==1,cancer%in%firstS)%>%select(casenum,cancer,yrdx,age,trt,sex)  
  D1$cancer=factor(D1$cancer,levels=firstS) # get rid of levels not in firstS
  D1=D1%>%filter(casenum%in%D2$casenum) # reduce firsts to just those with a second in D2 
  names(D1)[2:5]=c("cancer1","yrdx1","age1","trt1") #rename D1 cols so as not to join by them.
  D2=D2%>%select(casenum,cancer2=cancer,yrdx2=yrdx,age2=age) # reduce D2 to cols we want to slap on 
  D12=left_join(D2,D1,by="casenum") #Keeps all D2 rows, inserts missing where D1 doesn't match. 
  D12=D12%>%filter(!is.na(yrdx1)) # removes firsts before 1973
  D12=D12%>%mutate(yrdiffn=yrdx2-yrdx1)
  D12$yrdiffn[D12$yrdiffn==0]=0.33/12 # if first and second are in the same month, assume 1/3 of a month apart
  PY0=D0%>%mutate(py=surv) 
  PY0=PY0%>%filter(py>0)  #get rid of py=0  rows upfront
  PY0=PY0%>%mutate(agedx=age)
  # PY0$yrdx=floor(PY0$yrdx)
  PY0$cancer2="none"
  PY0=PY0%>%select(yrdx,agedx,sex,py,cancer1=cancer,cancer2,trt)
  PY12=D12%>%mutate(py=yrdiffn) 
  PY12=PY12%>%filter(py>0)  #get rid of py=0 rows upfront
  PY12=PY12%>%mutate(agedx=age1)
   # PY12$yrdx=floor(PY12$yrdx1)
  PY12$yrdx=PY12$yrdx1
  PY12=PY12%>%select(yrdx,agedx,sex,py,cancer1,cancer2,trt=trt1)
  d=rbind(PY0,PY12)
  d$cancer2=factor(d$cancer2)
  d$yeaR=d$yrdx
  d$yrdx=floor(d$yrdx)
  d
}
