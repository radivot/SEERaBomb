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
  OL=NULL
  for (icanc in cancS) {
    D=canc%>%filter(cancer==icanc)
    unlink(f<-paste0(outDir,"/",icanc,".xlsx"))
    wb <- loadWorkbook(f,create=T) 
    OL=NULL
    
    createSheet(wb, name = "quantiles")
    OL[["Q"]][["fu"]] =D%>%do(data.frame(t(quantile(.$surv))))
    names(OL[["Q"]][["fu"]])=c("min","Q1","median","Q3","max")
    OL[["Q"]][["fu"]]=cbind(D%>%summarize(N=n()),OL[["Q"]][["fu"]]) 
    writeWorksheet(wb, data.frame("Table 1. Median Follow Up in Months"), sheet = "quantiles",startRow=1,header=F)
    writeWorksheet(wb, OL[["Q"]][["fu"]], sheet = "quantiles",startRow=2)
    
    OL[["Q"]][["age"]]=D%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age"]])=c("min","Q1","median","Q3","max")
    OL[["Q"]][["age"]]=cbind(D%>%summarize(N=n()),OL[["Q"]][["age"]]) 
    writeWorksheet(wb, data.frame("Table 2. Median Age at Dx in Years"), sheet = "quantiles",startRow=5,header=F)
    writeWorksheet(wb, OL[["Q"]][["age"]], sheet = "quantiles",startRow=6)
    
    OL[["Q"]][["age.sex"]]=D%>%group_by(sex)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age.sex"]])=c("sex","min","Q1","median","Q3","max")
    OL[["Q"]][["age.sex"]]=left_join(D%>%group_by(sex)%>%summarize(N=n()),OL[["Q"]][["age.sex"]]) 
    writeWorksheet(wb, data.frame("Table 3. Median Age at Dx vs. Sex"), sheet = "quantiles",startRow=9,header=F)
    writeWorksheet(wb, OL[["Q"]][["age.sex"]], sheet = "quantiles",startRow=10)
    
    OL[["Q"]][["age.race"]]=D%>%group_by(race)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age.race"]])=c("race","min","Q1","median","Q3","max")
    OL[["Q"]][["age.race"]]=left_join(D%>%group_by(race)%>%summarize(N=n()),OL[["Q"]][["age.race"]]) 
    writeWorksheet(wb, data.frame("Table 4. Median Age at Dx vs. Race"), sheet = "quantiles",startRow=14,header=F)
    writeWorksheet(wb, OL[["Q"]][["age.race"]], sheet = "quantiles",startRow=15)
    
    OL[["Q"]][["age.sex.trt"]]=D%>%group_by(sex,trt)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age.sex.trt"]])=c("sex","trt","min","Q1","median","Q3","max")
    OL[["Q"]][["age.sex.trt"]]=left_join(D%>%group_by(sex,trt)%>%summarize(N=n()),OL[["Q"]][["age.sex.trt"]]) 
    writeWorksheet(wb, data.frame("Table 5. Median Age at Dx vs. Sex, Trt"), sheet = "quantiles",startRow=20,header=F)
    writeWorksheet(wb, OL[["Q"]][["age.sex.trt"]], sheet = "quantiles",startRow=21)
    
    OL[["Q"]][["age.sex.race"]]=D%>%group_by(sex,race)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age.sex.race"]])=c("sex","race","min","Q1","median","Q3","max")
    OL[["Q"]][["age.sex.race"]]=left_join(D%>%group_by(sex,race)%>%summarize(N=n()),OL[["Q"]][["age.sex.race"]]) 
    writeWorksheet(wb, data.frame("Table 6. Median Age at Dx vs. Sex, Race"), sheet = "quantiles",startCol=11,header=F)
    writeWorksheet(wb, OL[["Q"]][["age.sex.race"]], sheet = "quantiles",startCol=11,startRow=2)
    
    OL[["Q"]][["age.sex.race.trt"]]=D%>%group_by(sex,race,trt)%>%do(data.frame(t(quantile(.$agedx))))
    names(OL[["Q"]][["age.sex.race.trt"]])=c("sex","race","trt","min","Q1","median","Q3","max")
    OL[["Q"]][["age.sex.race.trt"]]=left_join(D%>%group_by(sex,race,trt)%>%summarize(N=n()),OL[["Q"]][["age.sex.race.trt"]]) 
    writeWorksheet(wb, data.frame("Table 7. Median Age at Dx vs. Sex, Race, Trt"), sheet = "quantiles",startCol=11,startRow=10,header=F)
    writeWorksheet(wb, OL[["Q"]][["age.sex.race.trt"]], sheet = "quantiles",startCol=11,startRow=11)
    
    
    createSheet(wb, name = "medOS")
    
    
    OL[["OS"]][["all"]] =medOS(D%>%group_by(cancer))
    writeWorksheet(wb, data.frame("Table 1. Median OS in Months"), sheet = "medOS",startRow=1,header=F)
    writeWorksheet(wb, OL[["OS"]][["all"]], sheet = "medOS",startRow=2)
    
    OL[["OS"]][["sex"]] =medOS(D%>%group_by(sex))
    writeWorksheet(wb, data.frame("Table 2. Median OS in Months vs. Sex"), sheet = "medOS",startRow=5,header=F)
    writeWorksheet(wb, OL[["OS"]][["sex"]], sheet = "medOS",startRow=6)
    
    OL[["OS"]][["race"]] =medOS(D%>%group_by(race))
    writeWorksheet(wb, data.frame("Table 3. Median OS in Months vs. Race"), sheet = "medOS",startRow=10,header=F)
    writeWorksheet(wb, OL[["OS"]][["race"]], sheet = "medOS",startRow=11)
    
    OL[["OS"]][["sex.race"]] =medOS(D%>%group_by(sex,race))
    writeWorksheet(wb, data.frame("Table 4. Median OS in Months vs. Sex, Race"), sheet = "medOS",startRow=16,header=F)
    writeWorksheet(wb, OL[["OS"]][["sex.race"]], sheet = "medOS",startRow=17)
    
    OL[["OS"]][["age"]]=medOS(D%>%group_by(age))
    writeWorksheet(wb, data.frame("Table 5. Median OS in Months vs. Age"), sheet = "medOS",startRow=25,header=F)
    writeWorksheet(wb, OL[["OS"]][["age"]], sheet = "medOS",startRow=26)
    
    OL[["OS"]][["year"]]=medOS(D%>%group_by(year))
    writeWorksheet(wb, data.frame("Table 6. Median OS in Months vs. Year"),sheet="medOS",
                   startRow=1,startCol=8,header=F)
    writeWorksheet(wb, OL[["OS"]][["year"]], sheet = "medOS",startRow=2,startCol=8)
    
    OL[["OS"]][["age.year"]]=medOS(D%>%group_by(age,year))
    writeWorksheet(wb, data.frame("Table 7. Median OS in Months vs. Age vs. Year"),sheet="medOS",
                   startRow=8,startCol=8,header=F)
    writeWorksheet(wb, OL[["OS"]][["age.year"]], sheet = "medOS",startRow=9,startCol=8)
    
    OL[["OS"]][["age.year.trt"]]=medOS(D%>%group_by(age,year,trt))
    writeWorksheet(wb, data.frame("Table 8. Median OS in Months vs. Age vs. Year, Trt"),sheet="medOS",
                   startRow=1,startCol=15,header=F)
    writeWorksheet(wb, OL[["OS"]][["age.year.trt"]], sheet = "medOS",startRow=2,startCol=15)
    
    saveWorkbook(wb)
    cat("Workbook was written to",f,"\n")
  }
  invisible(OL)
}
