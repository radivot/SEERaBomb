mkDemographics=function(canc,outDir="~/Results/SEERaBomb") {
  medianOS=te=hi=low=race=sex=age=agedx=year=cancer=trt=.=NULL
  # require(survival)
  medOS=function(X){
    d1=X%>%summarize(n=n())%>%mutate(prct=paste0(round(100*n/sum(n)),"%"))
    d2=X%>%do(te=summary(survfit(Surv(time=surv, event=COD!=0)~-1,data=.))$table[7:9])%>%mutate(medOS=te[[1]],low=te[[2]],hi=te[[3]])%>%
      mutate(medianOS=paste0("  ",medOS," (95% CI ",low,", ",hi,")"))
    bind_cols(d1,d2%>%select(medianOS))
  }
  
  if (!dir.exists(outDir)) dir.create(outDir,recursive=T)
  if (is.null(canc$cancer)) stop("cancer field is empty. Please include it!") 
  cancS=unique(as.character(canc$cancer))
  # hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="italic",border="Bottom")
  hs1=createStyle(fgFill="#DCE6F1",halign="CENTER",textDecoration="bold")
  # hs2=createStyle(fontColour="#ffffff",fgFill="#4F80BD",halign="center",valign="center", 
  #                 textDecoration="bold",border="TopBottomLeftRight")
  OL=NULL
  for (icanc in cancS) {
    D=canc%>%filter(cancer==icanc)
    unlink(f<-paste0(outDir,"/",icanc,".xlsx"))
    # wb <- loadWorkbook(f,create=T) 
    wb <- createWorkbook() 
    # createSheet(wb, name = "quantiles")
    addWorksheet(wb,"quantiles")
    OL[[icanc]][["Q"]][["fu"]] =D%>%do(data.frame(t(quantile(.$surv))))
    names(OL[[icanc]][["Q"]][["fu"]])=c("min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["fu"]]=cbind(D%>%summarize(N=n()),OL[[icanc]][["Q"]][["fu"]]) 
    writeData(wb, "quantiles",data.frame("Table 1. Median Follow Up in Months"), startRow=1,colNames=F)
    # writeData(wb, data.frame("Table 1. Median Follow Up in Months"), sheet = "quantiles",startRow=1,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["fu"]], startRow=2,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age"]]=D%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age"]])=c("min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age"]]=cbind(D%>%summarize(N=n()),OL[[icanc]][["Q"]][["age"]]) 
    writeData(wb,"quantiles", data.frame("Table 2. Median Age at Dx in Years"), startRow=5,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["age"]],startRow=6,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age.sex"]]=D%>%group_by(sex)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age.sex"]])=c("sex","min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age.sex"]]=left_join(D%>%group_by(sex)%>%summarize(N=n()),OL[[icanc]][["Q"]][["age.sex"]]) 
    writeData(wb,"quantiles", data.frame("Table 3. Median Age at Dx vs. Sex"), startRow=9,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["age.sex"]], startRow=10,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age.race"]]=D%>%group_by(race)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age.race"]])=c("race","min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age.race"]]=left_join(D%>%group_by(race)%>%summarize(N=n()),OL[[icanc]][["Q"]][["age.race"]]) 
    writeData(wb,"quantiles", data.frame("Table 4. Median Age at Dx vs. Race"), startRow=14,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["age.race"]], startRow=15,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age.sex.trt"]]=D%>%group_by(sex,trt)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age.sex.trt"]])=c("sex","trt","min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age.sex.trt"]]=left_join(D%>%group_by(sex,trt)%>%summarize(N=n()),OL[[icanc]][["Q"]][["age.sex.trt"]]) 
    writeData(wb,"quantiles", data.frame("Table 5. Median Age at Dx vs. Sex, Trt"), startRow=20,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["age.sex.trt"]], startRow=21,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age.sex.race"]]=D%>%group_by(sex,race)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age.sex.race"]])=c("sex","race","min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age.sex.race"]]=left_join(D%>%group_by(sex,race)%>%summarize(N=n()),OL[[icanc]][["Q"]][["age.sex.race"]]) 
    writeData(wb,"quantiles", data.frame("Table 6. Median Age at Dx vs. Sex, Race"), startCol=11,colNames=F)
    writeData(wb,"quantiles", OL[[icanc]][["Q"]][["age.sex.race"]], startCol=11,startRow=2,headerStyle = hs1)
    
    OL[[icanc]][["Q"]][["age.sex.race.trt"]]=D%>%group_by(sex,race,trt)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[[icanc]][["Q"]][["age.sex.race.trt"]])=c("sex","race","trt","min","Q1","median","Q3","max")
    OL[[icanc]][["Q"]][["age.sex.race.trt"]]=left_join(D%>%group_by(sex,race,trt)%>%summarize(N=n()),OL[[icanc]][["Q"]][["age.sex.race.trt"]]) 
    writeData(wb,"quantiles", data.frame("Table 7. Median Age at Dx vs. Sex, Race, Trt"),startCol=11,startRow=10,colNames=F)
    writeData(wb, "quantiles",OL[[icanc]][["Q"]][["age.sex.race.trt"]], startCol=11,startRow=11,headerStyle = hs1)
    
    
    # createSheet(wb, name = "medOS")
    addWorksheet(wb,"medOS")
    
    
    OL[[icanc]][["OS"]][["all"]] =medOS(D%>%group_by(cancer))
    writeData(wb, "medOS",data.frame("Table 1. Median OS in Months"), startRow=1,colNames=F)
    writeData(wb,"medOS", OL[[icanc]][["OS"]][["all"]], startRow=2,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["sex"]] =medOS(D%>%group_by(sex))
    writeData(wb, "medOS",data.frame("Table 2. Median OS in Months vs. Sex"), startRow=5,colNames=F)
    writeData(wb, "medOS",OL[[icanc]][["OS"]][["sex"]],  startRow=6,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["race"]] =medOS(D%>%group_by(race))
    writeData(wb,  "medOS",data.frame("Table 3. Median OS in Months vs. Race"), startRow=10,colNames=F)
    writeData(wb, "medOS", OL[[icanc]][["OS"]][["race"]],startRow=11,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["sex.race"]] =medOS(D%>%group_by(sex,race))
    writeData(wb, "medOS",data.frame("Table 4. Median OS in Months vs. Sex, Race"),  startRow=16,colNames=F)
    writeData(wb, "medOS",OL[[icanc]][["OS"]][["sex.race"]], startRow=17,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["age"]]=medOS(D%>%group_by(age))
    writeData(wb, "medOS",data.frame("Table 5. Median OS in Months vs. Age"), startRow=25,colNames=F)
    writeData(wb,  "medOS",OL[[icanc]][["OS"]][["age"]], startRow=26,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["year"]]=medOS(D%>%group_by(year))
    writeData(wb,"medOS", data.frame("Table 6. Median OS in Months vs. Year"),
                   startRow=1,startCol=8,colNames=F)
    writeData(wb, "medOS",OL[[icanc]][["OS"]][["year"]], startRow=2,startCol=8,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["age.year"]]=medOS(D%>%group_by(age,year))
    writeData(wb,"medOS", data.frame("Table 7. Median OS in Months vs. Age vs. Year"),
                   startRow=14,startCol=8,colNames=F)
    writeData(wb,"medOS", OL[[icanc]][["OS"]][["age.year"]],  startRow=15,startCol=8,headerStyle = hs1)
    
    OL[[icanc]][["OS"]][["age.year.trt"]]=medOS(D%>%group_by(age,year,trt))
    writeData(wb, "medOS",data.frame("Table 8. Median OS in Months vs. Age vs. Year, Trt"),
                   startRow=1,startCol=15,colNames=F)
    writeData(wb,"medOS", OL[[icanc]][["OS"]][["age.year.trt"]],startRow=2,startCol=15,headerStyle = hs1)
    
    # saveWorkbook(wb)
    saveWorkbook(wb,file=f,overwrite = TRUE)
    cat("Workbook was written to",f,"\n")
  }
  invisible(OL)
}
