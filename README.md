# SEERaBomb
This R package contains codes that setup SEER and A-bomb survivor data use with R. 
Current emphasis is on assessing the kinetics of risks of second cancers after treatments of first cancers. 
SEERaBomb does not work with SEER data that does not contain treatment information, 
i.e. you must obtain this extended dataset 
https://seer.cancer.gov/data/treatment.html
by signing an additional SEER data use agreement.

To install SEERaBomb use: `devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb")`

A-bomb data files lsshempy.csv and lssinc07.csv are obtained from https://www.rerf.or.jp/en/library/data-en/
under  “The incidence of leukemia, lymphoma and multiple myeloma among atomic bomb survivors: 1950-2001” and 
“Solid Cancer Incidence Data, 1958-1998”. Place these files in your local folder ~/data/abomb and run the following at the R prompt.   

mkAbomb()#converts files in ~/data/abomb into ~/data/abomb/abomb.RData
