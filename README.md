# SEERaBomb
To install SEERaBomb use: `devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb")`

This R package contains codes that setup SEER and A-bomb survivor data use with R. 
Current emphasis is on assessing the kinetics of risks of second cancers after treatments of first cancers. 
SEERaBomb does not work with SEER data that does not contain treatment information, 
i.e. you must obtain this extended dataset 
https://seer.cancer.gov/data/treatment.html
by signing an additional SEER data use agreement.

To set up SEER data use, unzip the custom radiation and chemotherapy ASCII text version of the SEER data into ~/data/SEER where ~ is your home directory. To use SEER data you must be familiar with field descriptions in ~/data/SEER/incidence/TextData.FileDescription.pdf.   
To convert your SEER ASCII text data into an R binary file, execute the following at the R prompt

```library(SEERaBomb)  #loads installed package SEERaBomb into memory<br> 
(df=getFields())    #gets SEER fields into a data frame<br> 
(rdf=pickFields(df))#picks a subset of SEER fields and defines their types<br> 
mkSEER(rdf)         #makes merged data file ~/data/SEER/mrgd/cancDef.Rdata```

Check the SEER data installation using 

```load("~/data/SEER/mrgd/cancDef.RData")#loads data.frame canc into memory```<br> 
```head(canc,3)                          #returns top 3 rows of canc```

To use the Japanese atomic bomb survivor data, obtain the files lsshempy.csv and lssinc07.csv from https://www.rerf.or.jp/en/library/data-en/
under  “The incidence of leukemia, lymphoma and multiple myeloma among atomic bomb survivors: 1950-2001” and 
“Solid Cancer Incidence Data, 1958-1998”,  place them in your folder ~/data/abomb, and run the following 

```mkAbomb() #converts files in ~/data/abomb into ~/data/abomb/abomb.RData```

As a check,

```load("~/data/abomb/abomb.RData")#loads data frames heme and solid```<br> 
```View(heme)                      #note descriptions under column names```

should show the hematological A-bomb data in a tab named heme in RStudio’s upper left panel.


To generate relative risk of mortality plots go the human mortality database  https://www.mortality.org/ and register. Then go to the bottom of  https://www.mortality.org/cgi-bin/hmd/hmd_download.php
and download the zip "All HMD countries"  and unzip it into ~/data/hmd_countries.  Now run 

```mkMrtLocal() #converts files in ~/data/hmd_countries/USA into ~/data/mrt/mrtUSA.RData``` 

To check the installation run

```load("~/data/mrt/mrtUSA.RData")  #loads R binary created by mkMortLocal```<br> 
```head(mrt$Female)   #shows first 5 years of female mortality rates 1933-2015```
