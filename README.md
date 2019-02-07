# SEERaBomb
This R package contains codes that setup SEER and A-bomb survivor data use with R. 
To install SEERaBomb use:  
```
devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb")
```

### Surveillance, Epidemiology, and End Results (SEER) Data
**WARNING:** SEERaBomb does not work with SEER data that does not contain treatment information, 
i.e. you must obtain this extended dataset 
https://seer.cancer.gov/data/treatment.html
by signing an additional SEER data use agreement.

To set up SEER data use in R, unzip the custom radiation and chemotherapy ASCII text version of the SEER data into ~/data/SEER where ~ is your home directory.  SEER ASCII data in ~/data/SEER/incidence and ~/data/SEER/populations is then converted into an R binary file as follows<br>
```
library(SEERaBomb)  #loads installed package SEERaBomb into memory 
(df=getFields())    #gets SEER fields into a data frame
(rdf=pickFields(df))#picks a subset of SEER fields and defines their types
mkSEER(rdf)         #makes merged data file ~/data/SEER/mrgd/cancDef.Rdata
```

Check the SEER data installation using 
```
load("~/data/SEER/mrgd/cancDef.RData")#loads data.frame canc into memory 
head(canc,3)                          #returns top 3 rows of canc
``` 


###  A-Bomb Survivor Data
To use the Japanese atomic bomb survivor data, obtain the files lsshempy.csv and lssinc07.csv from https://www.rerf.or.jp/en/library/data-en/
under  “The incidence of leukemia, lymphoma and multiple myeloma among atomic bomb survivors: 1950-2001” and 
“Solid Cancer Incidence Data, 1958-1998”,  place them in ~/data/abomb, and run  
```
mkAbomb() #converts files in ~/data/abomb into ~/data/abomb/abomb.RData
```

As a check,
```
load("~/data/abomb/abomb.RData")#loads data frames heme and solid 
View(heme)                      #note descriptions under column names
```
should show the hematological A-bomb data in a tab named heme in RStudio’s upper left panel.

### Human Mortality Database (HMD) Data
To generate relative risk of mortality plots go the human mortality database  https://www.mortality.org/ and register. Then go to the bottom of  https://www.mortality.org/cgi-bin/hmd/hmd_download.php
and download "All HMD countries" and unzip it into ~/data/hmd_countries.  Now run <br>
```
mkMrtLocal() #converts files in ~/data/hmd_countries/USA into ~/data/mrt/mrtUSA.RData
``` 

To check the installation run 
```
load("~/data/mrt/mrtUSA.RData")  #loads R binary created by mkMortLocal() 
head(mrt$Female)   #shows first 5 years of female mortality rates 1933-2015
```

### Usage

####Set Up
The code chunk below must be run before the examples. It loads libraries and data and defines acronyms.
```
graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb);library(ggsci)#load packages          
library(survival);library(survminer);library(bbmle)
load("~/data/SEER/mrgd/cancDef.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
load("~/data/abomb/abomb.RData")#load A-bomb data
load("~/data/mrt/mrtUSA.RData")#loads US mortality data
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("AML","ALL","CML")#second cancers of interest
gp=geom_point();gl=geom_line()
geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
gh=geom_hline(yintercept=1)
svts=scale_x_continuous(breaks=c(0,5,10))#surv times
agts=scale_x_continuous(breaks=c(25,50,75))#age times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
sy=scale_y_log10()
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
gxi=xlab("Age (Years)")
gyi=ylab(quote(paste("Cases per ",10^5," Person Years")))
```


####Example 1: CML mortality analyses

![](docs/tutFig1.png)

Chronic Myeloid Leukemia (CML) mortality RR time courses peak 4-6 years after diagnoses prior to 1990 (Figure 1A). This peak is not discernible in survival plots of the same data (Figure 1B). The R script for Figure 1A is: 
```
d=canc%>%filter(cancer=="CML")%>%print(n=13)
d%>%summarize(n=n(),na=sum(is.na(surv)),prct=100*na/n)#<2% missing
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)%>%print(n=13)
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1973,1990,2005,2015)))
gx=xlab("Years Since CML Diagnosis")
gy=ylab("Relative Risk of Mortality")
myt=theme(legend.text=element_text(size=12),strip.text=element_text(size=12))
D%>%ggplot(aes(x=t,y=RR,col=Years))+facet_grid(.~sex)+gp+gl+gx+gy+gh+
       svts+jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR+myt
ggsave("~/Results/tutorial/msdEx1A.pdf",width=4.5,height=3)
```

In the 1st line, %>% pipes the object to the left of it into the first argument of the function to the right of it.  Thus, canc is the first argument of filter(), and the argument shown determines which rows of canc to keep.  The filtered object is then piped to print() to show 13 lines. The 1st 10 have missing survivals. The 2nd line counts cases missing survival times to show that <2% are missing; the number of rows is returned by n(), is.na(surv) returns TRUE where surv is missing (NA), else FALSE, and sum() converts TRUE to 1 and FALSE to 0 before summing over the elements of its input vector. In the 3rd line mutate() creates `d$status` and overwrites `d$surv` to be in years, not months. For status, a non-zero cause of death (COD) code implies a death event, COD>0 converts a vector of integers into a vector of logicals, and as.numeric() converts FALSE to 0 and TRUE to 1. For surv, 0.5 is added to survival times in months because they were rounded down by SEER. The 4th line selects columns of interest. The 5th line calls SEERaBomb’s mortality-since-diagnosis function msd() with data frame d, which must include age, sex and year to match background mortality rates in mrt. Other msd() arguments are breakpoints of times-since-diagnosis (brkst) in years and year-of-diagnosis (brksy). The msd() output D has columns t (PY-weighted means of time intervals since diagnosis), RR, and Years (year-of-diagnosis interval); Years is a factor, i.e. a vector of integers where different integers are associated with different strings. D is then plotted to create Figure 1A.  In this call, facet_grid() accepts an argument of class formula, read as y~x, so .~sex indicates that in the plot layout, sex is in the x direction. Finally, in the last line, ggsave() saves the plot to a file; the type of file is inferred from the filename suffix. 

Figure 1B is then produced by this additional code:
```
labs=c("1973-1990","1991-2005","2006-2015")
d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),labels=labs))%>%print(n=13)
fit=survfit(Surv(surv,status)~yrg+sex,data=d) 
gy=ylab("Survival Probability")
ggsurvplot_facet(fit,d,facet.by="sex",legend.title="",xlim=c(0,12),
          short.panel.labs=T)+svts+jco+sbb+myt+gx+gy
ggsave("~/Results/tutorial/survEx1B.pdf",width=4.5,height=3)
```

The 2nd line uses cut() to bin a vector of numbers into levels of a factor; the R package base, which loads whenever R starts, provides cut(), print(), and other basic functions. The 3rd line uses survfit() and Surv() of the R package survival to generate survival probabilities for each sex and year group; Surv() combines time and event status and ~ specifies its dependence on year group (yrg) and sex. Figure 1B is then created using ggsurvplot_facet() of the R package survminer. 



###Cancer Types
The function mkSEER() assigns a cancer type, in a column named cancer, to each row in the data frame canc. The names and numbers of each cancer type is seen via:    
```
load("~/data/SEER/mrgd/cancDef.RData")
table(canc$cancer)
```
To see source code of the function mapCancs() that mkSEER() calls to define cancers from codes of International Classification of Disease (ICD) version 9 (ICD-9) and ICD Oncology version 3 (ICD-O3), type mapCancs at the R prompt. The mapCancs line `cancer[(histo3>=9840)&(histo3<9940)]="AML"` shows how acute myeloid leukemia (AML) is defined by ICD-O3 codes, and lines that follow show that acute promyelocytic leukemia (APL) and AML with other translocations or inversions (AMLti) are pulled out of this initial AML definition. 


### Statistics
SEERaBomb calculations of relative risks (RR) of second cancers are formed as observed cases (O) divided by expected cases (E), where E is based on background rates and person years (PY) at risk, matched on sex, age and year of diagnosis. RR of death are computed similarly, but with background mortality rates replacing background cancer incidence rates.  In both situations 95% confidence intervals (CI) of RR = O/E are formed assuming O is Poisson distributed: lower and upper limits are then `qchisq(.025, 2*O)/(2*E)` and `qchisq(.975, 2*O+2)/(2*E)`.



### Help
To learn more about a function() type ?function at the R prompt. Help pages reveal, for example, the default values of optional function arguments, e.g. ?pickFields shows
```
pickFields(sas,picks=c("casenum","reg","race","sex","agedx",
        "yrbrth","seqnum","modx","yrdx","histo3",
        "ICD9","COD","surv","radiatn","chemo"))
```
which states that the sas argument is required and that the picks argument can be omitted, in which case the default shown is used; sas is produced by getFields() using information in a SAS file SEER provides in ~/data/SEER/incidence. Examples of using pickFields() can be found in mkDataBinaries.R in the SEERaBomb installation folder SEERaBomb/doc/examples.   



