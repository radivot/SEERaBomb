install.packages("SEERaBomb")
install.packages("devtools")
devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb") 

library(SEERaBomb)   # loads the installed package SEERaBomb into memory
(df=getFields())     # gets SEER fields into a data frame
(rdf=pickFields(df)) # picks a subset of SEER fields and defines their types
mkSEER(rdf)          # makes merged data file ~/data/SEER/mrgd/cancDef.Rdata

load("~/data/SEER/mrgd/cancDef.RData") #loads the object canc into memory
head(canc,3)         # returns top 3 rows of canc


library(SEERaBomb)
mkAbomb() # converts files in ~\data\abomb into ~\data\abomb\abomb.RData 
To check it
load("~/data/abomb/abomb.RData") #loads A-bomb data frames
hemeDesc

mkMrt("username", "password") # sub in your account info

