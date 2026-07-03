mkSEERmrt<-function(seerHome="~/data/seer26",
                    inDir="csvs",
                    inFile="codRate.txt",
                    outFile="seerMrt.RData",
                    meanAgeFile="~/data/mrt/Ages.RData",
                    vars=c("COD","sex","a20","year","rate","num","denom")
                    ){
  # go into freq session with crude rate on mort data and let cod be pages, sex and age be rows, and year be a column,
  # rate, count and pop will then be a triplet within each matrix data cell, and the exported csv will have vars in this order
  ## from codRate.sas
  # label Causeofdeathrecodewith_COVID_19 = "Cause of death recode with COVID-19"
  # Sex = "Sex"
  # Agerecodewith1_year_olds_and_90 = "Age recode with <1 year olds and 90+"
  # Year_of_death = "Year of death"
  # Crude_Rate = "Crude Rate"
  # Count = "Count"
  # Population = "Population"
  # [Format=Cause of death recode with COVID-19]
  # 0=All Causes of Death
  # 1="  All Malignant Cancers"
  # 2="    Oral Cavity and Pharynx"
  # 3="      Lip"
  # 4="      Tongue"  
# ...
  # 70="    Leukemia"
  # 71="      Lymphocytic Leukemia"
  # 72="        Acute Lymphocytic Leukemia"
  # 73="        Chronic Lymphocytic Leukemia"
  # 74="        Other Lymphocytic Leukemia"
  # 75="      Myeloid and Monocytic Leukemia"
  # 76="        Acute Myeloid Leukemia"
  # 77="        Acute Monocytic Leukemia"
  # 78="        Chronic Myeloid Leukemia"
  # 79="        Other Myeloid/Monocytic Leukemia"
  # 80="      Other Leukemia"
  # 81="        Other Acute Leukemia"
  # 82="        Aleukemic, Subleukemic and NOS"
  # 83="    Miscellaneous Malignant Cancer"
  # 84="  In situ, benign or unknown behavior neoplasm"
  # 85="  Tuberculosis"
#...
  # 109="  Homicide and Legal Intervention"
  # 110="  COVID-19 (2020+)"
  # 111="  Other Cause of Death"
  
  # seerHome="~/data/seer26";inDir="csvs"
  # inFile="codRate.txt"
  # outFile="seerMrt.RData"
  # meanAgeFile="~/data/mrt/Ages.RData"
  # vars=c("COD","sex","a20","year","rate","num","denom")
  # require(dplyr)
  # require(forcats)
  # require(mgcv)
  # require(rgl)
  # require(purrr)
  COD=num=denom=year=sex=Ages=age=ageMid=a20=rate=rate2=W=NULL
  d=readr::read_csv(file.path(seerHome,inDir, inFile), col_names=vars,skip=0) 
  # p=readr::read_csv(file.path(seerHome,inDir, inFiles[2]), col_names=popVars,skip=1) 
  # d=dplyr::left_join(b,p)
  d=d%>%mutate(year=year+1968)
  d=d%>%filter(year!=1968)  #1968 is the sum over all years 
  d=d%>%filter(year>=1975)  
  # d=d%>%filter(year>=1999)  #icd9 to 10 starts here, but all causes and all leukemic causes should ride through; G6 less sure  
  d=d%>%filter(a20!=20)  #only known ages 
  d=d%>%mutate(ageMid=ifelse(a20==19,92.3,ifelse(a20==0,0.5,ifelse(a20==1,3,(a20-1)*5+2.5))))
  (d=d|>mutate(rate2=num/denom,sex=ifelse(sex==0,"Both",ifelse(sex==1,"Male","Female"))))
  tail(d)
  #load Ages, a list of two sexes of age-year matrices of PY-weighted 5-year bin age-group midpoints 
  load(meanAgeFile) # made in mrtAges.R
  getAge=function(a20,year,sex) Ages[[sex]][[as.character(a20),as.character(year)]]
  (d=d%>%mutate(age=pmap_dbl(list(a20,year,sex),getAge))) #update ages 
  save(d,file=file.path(seerHome,outFile)) # 336,000 × 7
}
 
