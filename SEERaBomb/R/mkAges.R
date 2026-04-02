mkAges<-function(mrtHome="~/data/mrt",country="USA"){
  # defaults above assume "~/data/mrt/mrtUSA.RData" was already made by SEERaBomb::mkMrtLocal()
  # mrt=num=denom=year=sex=Ages=age=rate=W=NULL
  mrt=Ages=NULL
  load(file.path(mrtHome,paste0("mrt",country,".RData"))) # most recent now up to 2023
  # load("~/data/mrt/mrtUSA.RData") #Human Mortality Database (HMD) goes 1933 to 2021 (89 years)
  # mrt loaded above is a list of two sexes, each with a matrix of 111 age rows (0 to 110+) and 91 year columns
  years=as.character(1975:2023)  # HMD goes up to 2023, SEER cancers only go up to 2022
  ages=as.character(0:19)  # new age group of 85-89 was added in May 2025 
  aN=length(ages)
  yN=length(years)
  (Z=matrix(rep(0,yN*aN),nrow=aN,dimnames=list(ages,years)))
  
  for (sex in c("Male","Female")){
    (M=mrt[[sex]][,years])
    (A=Z) # replace zeros with age midpoints
    for (y in years) {
      (m=M[,y])
      age=rep(0,19)
      (age[1]=exp(-m[1])/2)
      (age[2]=1+4*exp(-sum(m[2:5]))/2)
      for (i in 3:19) {
        strt=5*(i-2) # starting point
        mul=1
        tmp=0
        for (j in 1:5) {
          mul=mul*exp(-m[(5*(i-2)+j)]) #prob of alive at end of 1 Y
          tmp=tmp+mul/2
        }
        age[i]=strt+tmp
      }
      strt=90 # starting point
      mul=1
      tmp=0
      for (j in 1:15) { # use HMD data out to ages of 105 years
        mul=mul*exp(-m[(90+j)]) #prob of alive at end of one more year
        tmp=tmp+mul/2
      }
      age[20]=strt+tmp
      A[1:20,y]=age
    }
    Ages[[sex]]=A
  }
  save(Ages,file=file.path(mrtHome,paste0("Ages",country,".RData")))
  
}
 
