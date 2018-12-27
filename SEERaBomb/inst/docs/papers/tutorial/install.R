###install.R
install.packages("SEERaBomb")
install.packages("devtools")
devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb") 

library(SEERaBomb)  #loads installed package SEERaBomb into memory
(df=getFields())    #gets SEER fields into a data frame
(rdf=pickFields(df))#picks a subset of SEER fields and defines their types
mkSEER(rdf)         #makes merged data file ~/data/SEER/mrgd/cancDef.Rdata

load("~/data/SEER/mrgd/cancDef.RData")#loads data.frame canc into memory
head(canc,3)                          #returns top 3 rows of canc
table(canc$cancer) #reports numbers of each cancer type

mkAbomb()#converts files in ~/data/abomb into ~/data/abomb/abomb.RData
load("~/data/abomb/abomb.RData")#loads data frames heme and solid 
View(heme)                      #note descriptions under column names

mkMrt("username","password")#sub in your account info

install.packages(c("bbmle","survminer","ggsci","tidyverse"))
