library(devtools); install_github("radivot/SEERaBomb",subdir="SEERaBomb")
library(SEERaBomb)
rm(list=ls()) 
library(dplyr)
library(reshape2) 
library(inline)
(brks=c(0,0.25,1,3,5))  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #this is just to make a vector of tsx interval/row names 
getBinInfo(binS[3],binS) # test getBinInfo
getBinInfo(binS[1],binS)

brks=c(0,0.25,1,3,6)  
(binS=levels(cut(brks+0.1,breaks=c(brks,100)))) #make a vector of intervals 
survTimes=c(8,16,1.5,3.7)
getPY(survTimes,binS[1],binS,brks)# all contribute 0.25 to first interval 
getPY(survTimes,binS[4],binS,brks)# 3rd and 4th survivals contribute 0 and 0.7 to (3,6]
getPY(survTimes,binS[5],binS,brks)# 1st and 2nd survivals contribute 2 and 10 years to (6,100]

(mats=matrix(0,ncol=20,nrow=70))
(PYtest=structure(c(3.5, 11.25, 51.5, 58.5, 1974, 1976, 55, 69.75),.Dim = c(2L,4L), 
                  .Dimnames=list(c("1","10"),c("py", "ageL", "year", "ageR"))))
fillPYM(PYtest, mats)

# first run mkRRtsx (in examples) up to saving pm5 to a file, which is loaded here
system.time(load("~/Results/pm5.RData")) # 1 secs to load. 
#make sure year and age limits are right
range(pm5$D$age)
range(pm5$canc$age)
range(pm5$popsa$age)
range(pm5$D$year)
range(pm5$canc$year)
range(pm5$popsa$year)
#see what things look like MDS and CMML
library(rehape2)
D=pm5$D
EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
EI=lapply(EI,function(x) acast(x, age~year, value.var="Eincid")) # convert 3 cols of df to matrices
EI[["MDS"]]
EI[["CMML"]]

L=post1PYO(pm5$canc,brks=c(0,2,5),binIndx=1,Trt="rad" )
O=L$O
L$LPYM[["prostate"]]
L$LPYM[["MDS"]]
L$LPYM[["CMML"]]
E=getE2(L$LPYM,pm5$D) # make sure matrices are comformable
O/E



