rm(list=ls())
require(dplyr,quietly = TRUE,warn.conflicts = FALSE)
require(mgcv) # Mixed GAM Computation Vehicle with GCV/AIC/REML smoothness estimation

# skip this block if you only want to play with angles of plots
ptm <- proc.time()
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsa.RData") 
ageCut=15
canc=canc%.%filter(agedx>=ageCut,agedx<85)
popsa=popsa%.%filter(age86>=ageCut,age86<85)
for (Sex in c("male","female")) 
{
  cancS=canc%.%filter(sex==Sex)
  popsaS=popsa%.%filter(sex==Sex)
  cancS$cancer=factor(cancS$cancer) # get rid of opposite sex cancer types
  head(canc,2)
  picks=levels(cancS$cancer)
  L2D=vector(mode="list",length=length(picks)) 
  names(L2D)=picks
  L2Dp=L2D; D=NULL; 
  k=10  # knots. Maybe make this conditional on ageCut and i below
  for (i in picks) {
    if (i=="MDS")  {d=cancS%.%filter(cancer%in%i,yrdx>2000)
                    ps=popsaS%.%filter(year>2000)
    } else 
      if (i=="CMML")  {d=cancS%.%filter(cancer%in%i,yrdx>1985) 
                       ps=popsaS%.%filter(year>1985)
      } else  {
        d=cancS%.%filter(cancer%in%i)
        ps=popsaS
      }
    d=d%.%group_by(yrdx,age86)%.%summarise(cases=n())%.%mutate(year=yrdx)%.%select(-yrdx) 
    ps=ps%.%group_by(year,age86)%.%summarise(py=sum(py)) 
    X=left_join(ps,d)%.%mutate(age=age86,incid=1e5*cases/py)%.%select(-age86)
    X[is.na(X)]=0
    print(i)
    L2D[[i]]=gam(cases ~ s(age)+s(year)+ti(age,year,k=k)+offset(log(py)),
                 family=poisson(),data=X,method="REML") 
    L2Dp[[i]]=cbind(cancer=i,X,Ecases=exp(predict(L2D[[i]])),Eincid=1e5*exp(predict(L2D[[i]]))/X$py  )
    D=rbind(D,L2Dp[[i]])
  }
  save(L2D,D,file=paste0("~/ccf/tomR/",ifelse(Sex=="male","M","F"),"2D",ageCut,".RData")) 
} #sex
print(proc.time() - ptm)   


library(rgl)
library(dplyr)
for (Sex in c("male","female")) {
  load(paste0("~/ccf/tomR/",ifelse(Sex=="male","M","F"),"2D",ageCut,".RData")) 
  picks=names(L2D)
  head(D,2)
  for (i in picks) {
    pd=D%.%filter(cancer==i)%.%select(year,age,incid,Eincid)
    head(pd)
    sapply(pd,class)
    (nyrs=length(yrs<-unique(pd$year)))
    (length(ages<-unique(pd$age))*nyrs)
    pd$incid[pd$incid==0]=0.001
    with(pd,plot3d(age,year,log10(incid),xlab="",ylab="",zlab="")) #,sub=paste0(j," ",i,"s")))
    with(pd,surface3d(x=ages,y=yrs,z=matrix(log10(Eincid),ncol=nyrs),alpha=0.8,col="red"))
    clear3d(type="lights")
    light3d(theta = -90, phi = 75) 
    print(i)
    readline() # way to pause until input
    rgl.snapshot(filename=paste0("~/ccf/tomR/",i,ifelse(Sex=="male","M","F"),"2D",ageCut,".png"),fmt="png", top=TRUE)
  }
}

