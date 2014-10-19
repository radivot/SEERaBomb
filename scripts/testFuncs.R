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

