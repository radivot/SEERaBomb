#####  aggregate deaths into Max Resolution bins (i.e. only equivalent deaths stacks)
graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
load("pmf/data/d.RData") # made in demog.R
load("~/data/mrt/mrtUSA.RData")#loads US mortality data
(d=d%>%select(yrdx,agedx,sex,surv,status))
D=d%>%group_by(sex)%>%nest()
D$data[[1]]
D=D%>%mutate(ddata=map(data,function(x) x%>%filter(status==1)))
D=D%>%mutate(adata=map(ddata,function(x) x%>%group_by(agedx)%>%group_split()))
D
D$adata[[1]]
(fa=sort(unique(D$ddata[[1]]$agedx)))
length(fa) #64 unique death ages for females
sapply(D$adata[[1]],function(x) x$agedx[1]) # same, check

getBt=function(x) {
  ut=sort(unique(x$surv))
  c(0,ut[-length(ut)]+diff(ut)/2)
}  

D=D%>%mutate(brkst=map(adata,function(x) lapply(x,getBt)))
D
D$brkst[[1]]
getBa=function(x) {
  ua=sort(unique(x$agedx))
  c(0,ua[-length(ua)]+diff(ua)/2,100)
}

D=D%>%mutate(brksa=map(ddata,getBa))
D
mkAgeG=function(x,y) x%>%mutate(ageG=cut(agedx,breaks=y,include.lowest = T))
D=D%>%mutate(data=map2(data,brksa,mkAgeG))
D

D$data[[1]]
D  #done now with ddata and brksa so remove them
D=D%>%select(-ddata,-brksa)
D
D=D%>%mutate(tbL=map(data,function(x) x%>%group_by(ageG)%>%group_split()))
D # dims check, remove adata
D=D%>%select(-adata)
D$tbL[[1]][[1]] #need sex back in here for msd to work
D=D%>%mutate(tbL=map2(tbL,sex,function(x,y) map2(x,y,function(x,y) x%>%mutate(sex=y)) ))
D$tbL[[1]][[1]] #got it
D$tbL[[2]][[1]] #got it

getMrt=function(x,y) mapply(function(x,y) msd(x,mrt,brkst=y),x,y,SIMPLIFY = FALSE)
D=D%>%mutate(m=map2(tbL,brkst,getMrt)) # takes a while
D
D$m[[1]][[1]]
D$tbL[[1]][[1]]  # need to move the mean age from here into m
D=D%>%mutate(ma=map2(m,tbL,function(x,y) map2(x,y,function(x,y) x%>%mutate(a=mean(y$agedx))) ))
D$ma[[1]][[1]] #got it
D  # now we just need to rbind ma
(m=bind_rows(D$ma))
m=m%>%select(-(EAR:rrU))
save(m,file="pmf/data/maxRes.RData")
