# ############ this builds the SEER data to include primSite
library(SEERaBomb)
(df=getFields("~/data/SEER")) # this doesn't fly in windows since ~ maps to /users/radivot/documents
picks=c("casenum","reg","race","sex","agedx",
        "yrbrth","seqnum","modx","yrdx","histo3",
        "ICD9","primsite","COD","surv","radiatn","chemo")
(rdf=pickFields(df,picks))
mkSEER(rdf,outFile="cancPrim",writePops=F)

