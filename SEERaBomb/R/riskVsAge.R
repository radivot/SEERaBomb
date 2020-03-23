riskVsAge=function(canc,firstS=c("NHL","HL","MM"),secondS=c("AML","MDS"),brksa=c(0,30,50,60,70,80)) {
 COD=L=U=age=age1=ageL=agec=agedx=cancer=cancer1=cancer2=casenum=modx=rad=chemo=ntrt=NULL
   o=py=qpois=seqnum=sex=surv=trt=trt1=year=yrbrth=yrdiffn=yrdx=yrdx1=yrdx2=NULL
  brksa=c(brksa,126)
  brksm=brksa[-length(brksa)] + diff(brksa)/2
  brksm[length(brksm)]=brksa[length(brksa)-1]+5 # use this for labels in cut of attained ages later
  canc=canc%>%mutate(year=yrdx) 
  canc=canc%>%mutate(yrdx=round(yrdx+(modx-0.5)/12,3))    #modx=1=January 
  canc=canc%>%mutate(surv=round((surv+0.5)/12,3))%>%  
    mutate(age=agedx+0.5) #convert ages at diagnosis to best guesses
  canc=canc%>%select(casenum,yrbrth,sex,age,seqnum,year,yrdx,surv,COD,trt,cancer) 
  head(canc)
  trtS=levels(canc$trt)
  D2=canc%>%filter(seqnum==2) # D2 holds second primaries
  D0=canc%>%filter(seqnum==0,surv<200,cancer%in%firstS)
  D0$cancer=factor(D0$cancer,levels=firstS) # get rid of levels not in firstS. 
  head(D0,2)
  table(D0$cancer)
  D1=canc%>%filter(seqnum==1,cancer%in%firstS)%>%select(casenum,cancer,yrdx,age,trt,sex)  
  D1$cancer=factor(D1$cancer,levels=firstS) # get rid of levels not in firstS
  D1=D1%>%filter(casenum%in%D2$casenum) # reduce firsts to just those with a second in D2 
  head(D1)
  names(D1)[2:5]=c("cancer1","yrdx1","age1","trt1") #rename D1 cols so as not to join by them.
  D2=D2%>%select(casenum,cancer2=cancer,yrdx2=yrdx,age2=age) # reduce D2 to cols we want to slap on 
  D12=left_join(D2,D1,by="casenum") #Keeps all D2 rows, inserts missing where D1 doesn't match. 
  D12=D12%>%filter(!is.na(yrdx1)) # removes firsts before 1973
  D12=D12%>%mutate(yrdiffn=yrdx2-yrdx1)
  D12$yrdiffn[D12$yrdiffn==0]=0.33/12 # if first and second are in the same month, assume 1/3 of a month apart
  head(D12)
  PY0=D0%>%mutate(py=surv) 
  head(PY0)
  PY0=PY0%>%filter(py>0)  #get rid of such rows upfront
  PY0=PY0%>%mutate(ageL=age) 
  PY0$year=floor(PY0$yrdx)
  PY0$cancer2="none"
  PY0=PY0%>%select(cancer1=cancer,cancer2,py,ageL,year,trt,sex)
  PY12=D12%>%mutate(py=yrdiffn) 
  PY12=PY12%>%filter(py>0)  #get rid of py=0 rows upfront
  PY12=PY12%>%mutate(ageL=age1)
  head(PY12)
  PY12$year=floor(PY12$yrdx1)
  PY12=PY12%>%select(cancer1,cancer2,py,ageL,year,trt=trt1,sex)
  d=rbind(PY0,PY12)
  
  D=NULL #will stack these 
  yrs=1975:max(canc$yrdx); ages=0.5:125.5  # used to initiate PYM with zeros (need 1973 start for fillPYM)
  
  for (ii in c("Male","Female"))
    for (i in firstS)
      for (j in trtS) {
      # for (j in c("rad","noRad")) {
        # PYin=d%>%filter(cancer1==i,trt==j)%>%select(py=surv,ageL=agedx,year=yrdx)
        PYin=d%>%filter(sex==ii,cancer1==i,trt==j)%>%select(py,ageL,year)
        head(PYin)
        PYin=as.matrix(PYin)
        PYM=matrix(0,ncol=length(yrs),nrow=length(ages),dimnames=list(ages,yrs)) 
        SEERaBomb::fillPYM(PYin,PYM)
        # PYM
        # k="AML"
        for (k in secondS) {
          # head(d)
          O=d%>%filter(sex==ii,cancer1==i,cancer2==k,trt==j)%>%mutate(ageL=floor(ageL+py),year=round(year+py),py=1)%>%
            select(py,ageL,year)
          head(O)
          Oin=as.matrix(O)
          OM=matrix(0,ncol=length(yrs),nrow=length(ages),dimnames=list(ages,yrs)) 
          SEERaBomb::fillPYM(Oin,OM)
          # OM
          # print(sum(OM))
          tD=data.frame(age=0.5:125.5,o=apply(OM,1,sum),py=apply(PYM,1,sum))
          # tD=tD%>%mutate(agec=cut(age,c(0,50,65,126),labels=c(25,57.5,70))) %>%
          tD=tD%>%mutate(agec=cut(age,brksa)) %>%
            group_by(agec) %>%
            summarise(py=sum(py)/1e5,o=sum(o),
                      L=qpois(.025,o),U=qpois(.975,o),Incid=o/py,IL=L/py,IU=U/py,trt=j,cancer1=i,cancer2=k,sex=ii)
          D=rbind(D,tD)
        }
      }
  D
  # D$trt[D$trt=="rad"]="Radiation"
  # D$trt[D$trt=="noRad"]="No Radiation"
  # D$trt=factor(D$trt,levels=c("Radiation","No Radiation"))
  D$trt=factor(D$trt) # flipped, rad is good, so take natural order
  D$ntrt=D$trt%>%str_replace_all("no","No ")
  D=D%>%separate(ntrt,c("rad","chemo"),sep="[\\.]",fixed=T)
  D=D%>%mutate_at(vars(rad:chemo),funs(str_to_title))
  D$cancer2=factor(D$cancer2)
  D$age=brksm[as.numeric(D$agec)] 
  D
}
