rm(list=ls())
require(dplyr)
library(SEERaBomb)
# (df=getFields("~/data/SEER")) # this doesn't fly in windows since ~ maps to /users/radivot/documents
(df=getFields("/Users/radivot/data/SEER")) # so use absolute path. Here df holds the full set of data field (df) options
(rdf=pickFields(df)) # this is a reduced data field (rdf) dataframe (note fewer rows AND one extra integer/string column)
#if you get "Error in pickFields(df) : The following picks are not allowed: radiatn, chemo" you probably downloaded
# the regular SEER data. You must get the Custom data that includes radiation and chemo information in it. 
#You MUST get the data from  https://seer.cancer.gov/data/treatment.html
#DO NOT get the data from https://seer.cancer.gov/data/options.html
mkSEER(rdf,seerHome="/Users/radivot/data/SEER") #merges all cancer binaries into ~/data/SEER/mrgd/cancDef.RData (Def for default picks)

# these are the default picks for pickFields (see its help page)
defPicks=c("casenum","reg","race","sex","agedx",
        "yrbrth","seqnum","modx","yrdx","histo3",
        "ICD9","COD","surv","radiatn","chemo")

## STAGES: if you want to explore different stage fields, you may want something like this
stageFields=c("eod10sz","eod10nd","cstumsiz","dajcct","dajccn","dajccm",
        "siterwho","hststga","ajccstg","aj3seer","dajcc7t","dajcc7n","dajcc7m","dajcc7stg")
picks=c(defPicks,stageFields)
(rdf=pickFields(df,picks))
mkSEER(rdf,outFile="cancStgs",writePops=F) #80 secs + 120 secs


## ALL COLUMNS ######## make one with all columns just to see how big things 
#get. The following defeats one of the main points of this package, which is to 
#gain speed by focusing only on fields of interest. The resulting binaries are 
#thus not likely to be useful on a regular basis, but may be useful from a
#computer programming perspective for checking out speed in the limit of using all fields.
rdf=pickFields(df,picks=df$names)
#2016+ fields are crashing because they are not integers, this fixes it
rdf=pickFields(df,picks=setdiff(df$names,c("tumsizs","dsrpsg","dasrct","dasrcn","dasrcm","dasrcts","dasrcns","dasrcms","tnmednum","metsdxln","metsdxo")))
mkSEER(rdf,outFile="cancAll",writePops=F) #149 secs # runs out of memory now
# making the SQL db was an additional  ~4 minutes. 

# If you want to check to see what fields you have in a binary right now, you can do this
system.time(load("~/data/SEER/mrgd/cancDef.RData")) # 3 secs to load 10M cases is fast relative to having all fields
head(canc,2)
system.time(load("~/data/SEER/mrgd/cancALL.RData")) # which almost takes 30 secs
head(canc,2)

# #The following can be ignored. It is a note from TR to TR on how to recreate an
# #old structure of SEER binaries still needed to run certain old scripts of his.
# (df=getFields("/Users/radivot/data/SEER")) 
# (rdf=pickFields(df))
# for (i in c("73","92","00")) mkSEERold(rdf,dataset=i,mkDFs=T) #(old way) populates binaries into these folders
