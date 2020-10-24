graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse)
library(SEERaBomb)
library(mgcv)
# # https://www.census.gov/2010census/xls/fips_codes_website.xls
# # library(gdata)
# # sc=read.xls("pmf/data/fips_codes_website.xls")
# # save(sc,file="pmf/data/fips.RData")
load("pmf/data/fips.RData")
# load("pmf/data/allfields2019.RData") # 4 missing surv already out in allFields.R
# d=d%>%filter(yrdx>2000)%>%print(n=2)
# d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
# d7=d%>%select(yrdx,agedx,sex,surv,status,stcnty,reg)
# save(d7,file="pmf/data/d7.RData")
# load("pmf/data/d7.RData")
# (d7=d7%>%select(sex,yrdx,agedx,surv,status,reg,stcnty))
# load("pmf/data/Ddp.RDA")
# unique(Dm$tg)
# unique(Df$tg)
# brkst<-c(0,0.5,1,1.6,2.4,3.2,4.5,6.5) #rough average of M and F
# unique(Dm$ag)
# unique(Df$ag)
# brksa<-c(0,57,63,68,72,76,80,84,100)
# (d=d7%>%mutate(ageG=cut(agedx,brksa,include.lowest=TRUE)))
# (d=d%>%group_by(ageG,reg,stcnty)%>%nest())
# d$data[[1]]
# (d=d%>%mutate(a=map_dbl(data,function(x) mean(x$agedx))))
# load("~/data/mrt/mrtUSA.RData")#loads US mortality data
# (d=d%>%mutate(m=map(data,function(x) msd(x,mrt,brkst)))) # takes a while
# (D=d%>%select(-data)%>%unnest(m))
# save(d,D,file="pmf/data/dTabs.RData")
load("pmf/data/dTabs.RData")
(D=D%>%select(reg,stcnty,a,t:PY,s)%>%arrange(s,a,t))
Dm=D%>%filter(s=="Male")
Df=D%>%filter(s=="Female")
load("pmf/data/models.RDA")
Dm$e=exp(predict(bm,newdata=Dm)) 
Df$e=exp(predict(bf,newdata=Df)) 
D=bind_rows(Dm,Df)


getOE=function(DD) {
  DD=DD%>%group_by(reg,stcnty)%>%summarize(O=sum(O),E=sum(e),RR=O/E)
  DD$P=mapply(function(O,E) poisson.test(O, T = E)$p.value,DD$O,DD$E)
  DD%>%arrange(P)%>%filter(P<0.05,O>10)
  # DD%>%filter(RR<1,P<0.05)
}
(Cntys=getOE(D))
#   reg   stcnty     O     E    RR       P
# 1 NJ     34003    22 37.7  0.584 0.00882
# 2 sW     53033    74 97.0  0.763 0.0193 
# 3 LA     22019    11  5.09 2.16  0.0216 
# 4 LA     22017    14  7.35 1.90  0.0240 
# 5 CA      6019    27 17.5  1.55  0.0301 

library(rateratio.test)
rateratio.test(c(22,149),c(37.7,112.6))

### good places to live
(sc%>%filter( State.FIPS.Code==34,County.FIPS.Code==3))$GU.Name # Bergen County,NJ next to NYC
(sc%>%filter( State.FIPS.Code==53,County.FIPS.Code==33))$GU.Name # Seattle metro area
### bad places to live
(sc%>%filter( State.FIPS.Code==22,County.FIPS.Code==19))$GU.Name # Lake Charles, sulphur (west end of state) 
(sc%>%filter( State.FIPS.Code==22,County.FIPS.Code==17))$GU.Name # Shreveport
(sc%>%filter( State.FIPS.Code==6,County.FIPS.Code==19))$GU.Name # Fresno
library(WriteXLS)
WriteXLS(Cntys,ExcelFileName="pmf/outs/Table1.xlsx")


getOEr=function(DD) {
  DD=DD%>%group_by(reg)%>%summarize(O=sum(O),E=sum(e),RR=O/E)
  DD$P=mapply(function(O,E) poisson.test(O, T = E)$p.value,DD$O,DD$E)
  # DD%>%filter(RR<1,P<0.1)
  DD%>%arrange(P,RR)
}
(Regs=getOEr(D))
#   reg       O       E    RR        P
#  1 LA      149 113.    1.32  0.000954
#  2 CA      578 538.    1.07  0.0886  
#  3 GA      154 135.    1.14  0.101   
#  4 sW      213 238.    0.893 0.105   
#  5 NJ      292 316.    0.923 0.177   
#  6 HI       63  75.1   0.839 0.184   
#  7 la      305 329.    0.926 0.186   
#  8 aG       57  47.8   1.19  0.192   
#  9 AK        2   0.817 2.45  0.197   
# 10 KY      140 128.    1.09  0.310   
# 11 dM      187 200.    0.936 0.396   
# 12 sj       64  71.8   0.892 0.408   
# 13 UT       42  47.9   0.878 0.469   
# 14 CT      110 104.    1.06  0.523   
# 15 IA      177 170.    1.04  0.565   
# 16 sf      119 122.    0.973 0.821   
# 17 NM       56  54.8   1.02  0.839   
# 18 rG        5   5.94  0.842 1       
WriteXLS(Regs,ExcelFileName="pmf/outs/Table0Regs.xlsx")

getOEr(Dm)
getOEr(Df)  # LA holds for both sexes


getOEc=function(DD) {
  DD=DD%>%group_by(s,reg,stcnty)%>%summarize(O=sum(O),E=sum(e),RR=O/E)
  DD$P=mapply(function(O,E) poisson.test(O, T = E)$p.value,DD$O,DD$E)
  DD%>%filter(RR<1,P<0.1)%>%arrange(P)
}
getOEc(Dm)
getOEc(Df)  #NJ and sW tops of lists for both sexes


