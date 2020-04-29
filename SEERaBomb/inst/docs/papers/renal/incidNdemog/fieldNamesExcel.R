rm(list=ls())
require(dplyr)
library(SEERaBomb)
(df=getFields("~/data/SEER")) # this doesn't fly in windows since ~ maps to /users/radivot/documents
library(WriteXLS)
WriteXLS(df,ExcelFileName="renal/outs/fieldDefs2019.xlsx")
