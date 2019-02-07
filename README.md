# SEERaBomb
This R package contains codes that setup SEER and A-bomb survivor data use with R. 
To install SEERaBomb use: <br> `devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb")`

### Surveillance, Epidemiology, and End Results (SEER) Data
**WARNING:** SEERaBomb does not work with SEER data that does not contain treatment information, 
i.e. you must obtain this extended dataset 
https://seer.cancer.gov/data/treatment.html
by signing an additional SEER data use agreement.

To set up SEER data use in R, unzip the custom radiation and chemotherapy ASCII text version of the SEER data into ~/data/SEER where ~ is your home directory.  SEER ASCII data in ~/data/SEER/incidence and ~/data/SEER/populations is then converted into an R binary file as follows
```library(SEERaBomb)  #loads installed package SEERaBomb into memory```<br> 
```(df=getFields())    #gets SEER fields into a data frame```<br> 
```(rdf=pickFields(df))#picks a subset of SEER fields and defines their types```<br> 
```mkSEER(rdf)         #makes merged data file ~/data/SEER/mrgd/cancDef.Rdata```

Check the SEER data installation using <br>
```load("~/data/SEER/mrgd/cancDef.RData")#loads data.frame canc into memory```<br> 
```head(canc,3)                          #returns top 3 rows of canc```

###  A-Bomb Survivor Data
To use the Japanese atomic bomb survivor data, obtain the files lsshempy.csv and lssinc07.csv from https://www.rerf.or.jp/en/library/data-en/
under  “The incidence of leukemia, lymphoma and multiple myeloma among atomic bomb survivors: 1950-2001” and 
“Solid Cancer Incidence Data, 1958-1998”,  place them in ~/data/abomb, and run the following 
```mkAbomb() #converts files in ~/data/abomb into ~/data/abomb/abomb.RData```

As a check,<br/>
```load("~/data/abomb/abomb.RData")#loads data frames heme and solid```<br> 
```View(heme)                      #note descriptions under column names```<br/>
should show the hematological A-bomb data in a tab named heme in RStudio’s upper left panel.

### Human Mortality Database (HMD) Data
To generate relative risk of mortality plots go the human mortality database  https://www.mortality.org/ and register. Then go to the bottom of  https://www.mortality.org/cgi-bin/hmd/hmd_download.php
and download "All HMD countries" and unzip it into ~/data/hmd_countries.  Now run <br>
```mkMrtLocal() #converts files in ~/data/hmd_countries/USA into ~/data/mrt/mrtUSA.RData``` 

To check the installation run <br>
```load("~/data/mrt/mrtUSA.RData")  #loads R binary created by mkMortLocal()```<br> 
```head(mrt$Female)   #shows first 5 years of female mortality rates 1933-2015```
