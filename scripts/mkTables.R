rm(list=ls())
require(dplyr)
# require(ggplot2)
library(XLConnect)
for (Sex in c("male","female")) {
  unlink(f<-paste0("/Users/radivot/ccf/tomR/",Sex,".xlsx"))
  wb <- loadWorkbook(f,create=T) 
  #   load(paste0("~/ccf/tomR/",ifelse(Sex=="male","M","F"),"2D2_5_10_15.RData")) 
  load(paste0("~/ccf/tomR/",ifelse(Sex=="male","M","F"),"2D0_0.5_1_2_5_10_15.RData")) 
  intvs=names(L[["0"]][["Obs"]]) 
  picks=rownames(L[["0"]][["Obs"]][[intvs[1]]])
  for (icanc in picks) {
    #    icanc="prostate"
    createSheet(wb, name = icanc)
    M=NULL
    for (R in c("0","16")) {
      D=NULL
      for (intv in intvs) {
        O=L[[R]][["Obs"]][[intv]][icanc,,drop=F]
        E=L[[R]][["Exp"]][[intv]][icanc,,drop=F]
        RR=O/E
        LL=qchisq(.025,2*O) /(2*E)
        UL=qchisq(.975,2*O+2)/(2*E)
        col=as.data.frame(round(cbind(t(RR),t(LL),t(UL),O=t(O),E=t(E)),2))
        names(col)=c("RR","LL","UL","O","E")
        D=cbind(D,paste0(col$RR,"(",col$LL,",",col$UL,") O=",O))
        #     print(R)
      }
      #   D
      colnames(D)=paste0(intvs,ifelse(R=="0"," after no IR"," after IR"))
      rownames(D)=rownames(col)
      M=cbind(M,D)
    } #rad
    #     writeWorksheet(wb, data.frame("second cancer"=picks,M), sheet = icanc,rownames=1)
    writeWorksheet(wb, cbind("2nd cancer"=picks,M), sheet = icanc,rownames=1)
    setColumnWidth(wb,sheet = icanc, column = 1, width = 2500)
    for (j in 2:(dim(M)[2]+1)) setColumnWidth(wb,sheet = icanc, column = j, width = 4700)
#     for (j in 1:(dim(M)[2]+1)) setColumnWidth(wb,sheet = icanc, column = j, width = 5600)
    saveWorkbook(wb)
  } #icanc
} #Sex
