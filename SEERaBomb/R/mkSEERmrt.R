mkSEERmrt<-function(seerHome="~/data/seer25",inDir="csvs",inFiles=c("cod111.txt","pops.txt"),
               outFile="seerMrt.RData",
               meanAgeFile="~/data/mrt/Ages.RData",
               vars=c("sex","age","COD","year","num"),
               popVars=c("sex","age","year","denom")){

  # Go into SEER*stat and use the mortality data to make cod111.txt containing
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
  
# and pops.txt containing
  # [System]
  # Output filename=C:\Users\radivot\Documents\pops.csv
  # Matrix filename=pops Matrix-7
  # Database name=Populations - Total U.S. (1969-2023) <Katrina/Rita Adjustment> - Linked To County Attributes - Total U.S., 1969-2023 Counties
  # Title1=population denoms for incid and mortality

  
  # seerHome="~/data/seer25";inDir="csvs"
  # inFiles=c("cod111.txt","pops.txt")
  # outFile="seerMrt.RData"
  # meanAgeFile="~/data/mrt/Ages.RData"
  # vars=c("sex","age","COD","year","num")
  # popVars=c("sex","age","year","denom")
  # require(dplyr)
  # require(forcats)
  # require(mgcv)
  # require(rgl)
  # require(purrr)
  COD=num=denom=year=sex=Ages=age=rate=W=NULL
  b=readr::read_csv(file.path(seerHome,inDir, inFiles[1]), col_names=vars,skip=1) 
  p=readr::read_csv(file.path(seerHome,inDir, inFiles[2]), col_names=popVars,skip=1) 
  d=dplyr::left_join(b,p)
  (d=d|>mutate(rate=num/denom,year=year+1975,sex=ifelse(sex==0,"Male","Female")))
  tail(d)
  #load Ages, a list of two sexes of age-year matrices of PY-weighted 5-year bin age-group midpoints 
  load(meanAgeFile) # made in mrtAges.R
  getAge=function(age,year,sex) Ages[[sex]][[as.character(age),as.character(year)]]
  (d=d%>%mutate(age=pmap_dbl(list(age,year,sex),getAge))) #update ages 
  save(d,file=file.path(seerHome,outFile)) # 219,520 × 7
}
 
