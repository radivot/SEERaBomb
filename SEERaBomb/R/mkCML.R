mkCML<-function(seerHome="~/data/seer25",inDir="csvs",inFiles=c("s8.txt",
                                "s12.txt","s17.txt"),
                outFile="cml.RData",vars=c("id","agedx","sex","race","yrdx",
                       "who","histo3","surv","COD","rad","chemo")){
  # Go into SEER*stat, and for SEER 8, 12 and 17, using plus, make these case-listing columns
  # Patient_ID
  # Agerecodewithsingle_ages_and_90
  # Sex
  # Race_recode_White_Black_Other
  # Year_of_diagnosis
  # Site_recode_ICD_O_3_WHO_2008
  # Histologic_Type_ICD_O_3
  # Combined_Summary_Stage_2004
  # Survival_months
  # COD_to_site_recode
  # Radiation_recode
  # Chemotherapy_recode_yes_no_unk

  # seerHome="~/data/seer25";inDir="csvs";infiles=c("s8.txt","s12.txt","s17.txt");outFile="cml.RData"
  # vars=c("id","agedx","sex","race","yrdx","who","histo3","surv","COD","rad","chemo")  
  
  # require(dplyr)
  # require(readr) 
  # require(forcats) 
  # outDir="mrgd";outFile="cancDef";writePops=T;writeRData=TRUE;writeDB=TRUE  # for debugging
  # indices = list(c("sex","race"), "histo2", "histo3", "ICD9")
  
  # gimmick to get rid of unwanted notes in R CMD check
  sex=agedx=histo3=surv=yrdx=rad=chemo=cancer=seqnum=who=race=cancerW=NULL
  L=NULL
  for (i in 1:3) {
    L[[i]]=readr::read_csv(file.path(seerHome,inDir, inFiles[i]), col_names=vars,skip=1) 
  }
  # system.time(d8 <-read_csv("~/data/seer25/csvs/s8.txt", col_names=vars,skip=1)) 
  # system.time(d12<-read_csv("~/data/seer25/csvs/s12.txt",col_names=vars,skip=1))
  # system.time(d17<-read_csv("~/data/seer25/csvs/s17.txt",col_names=vars,skip=1)) # 2 secs (uses multi-cores)
  # d=bind_rows(d8,d12,d17) # need patient ID to make each row unique
  d=bind_rows(L) # need patient ID to make each row unique
  d=distinct(d)
  table(d$COD)
  d=d%>%mutate(histo3=8000+histo3)
  d=d%>%mutate(yrdx=yrdx+1800,status=as.numeric(COD>0),surv=(surv+0.5)/12)
  d=d%>%filter(surv<200)  # clip off surv = 9999 months = 833 years
  d=d%>%mutate(sex=ifelse(sex==1,"Male","Female"))
  d=d%>%mutate(sex=forcats::as_factor(sex))
  head(d)
  COD=d$COD #start with vec of integers. Map to a vec of Strings
  CODS=rep("UNK",dim(d)[1]) #set default to "unknown" type of death
  CODS[COD==0]="alive"
  CODS[(COD>=1)&(COD<=73)|(COD==86)|(COD==90)]="CA"
  CODS[(COD>=74)&(COD<=85)|(COD==89)]="LC"
  CODS[COD==130]="CA" #in situ (benign)
  CODS[(COD>=133)&(COD<=145)]="IN" # infection
  CODS[COD==148]="DK"  # diabetes
  CODS[COD==151]="YOC"  # alzheimers -> yet other causes
  CODS[COD==154]="CV"  # heart disease
  CODS[COD==157]="CV" # hypertension without HD
  CODS[COD==160]="CV"  #cerebroVasc"
  CODS[COD==163]= "CV" #"athero"
  CODS[COD==166]= "CV"     #"aoritic aneurysm"
  CODS[COD==169]= "CV"  #"other disease of Vasc"
  CODS[COD==172]= "IN" #"pneumonia"
  CODS[COD==175]="YOC" # COPD, no signal so smoking makes both. chronic obstructive pulminary disease
  CODS[COD==178]="YOC" # ulcer
  CODS[COD==181]="YOC" # liver disease
  CODS[COD==184]="DK" # kidney disease
  CODS[COD==199]="ASH" #"accidents"
  CODS[COD==202]="ASH"  #"suicide"
  CODS[COD==205]="ASH" # homocide"
  CODS[COD%in%c(187,190,193)]="YOC" # perinatal conditions
  CODS[COD%in%c(196,208,252)]="YOC" # other causes, including ill-defined and unknown
  # CODS[COD==252]="UNK" # same if no comment => all accounted for
  d$COD6=as.factor(CODS)
  
  COD=d$COD #repeat for just two types of death, LC and OC
  CODS=character(dim(d)[1]) #initialize string (S) version of COD to all ""
  CODS[(COD>=74)&(COD<=85)|(COD==89)]="LC"
  CODS[(COD<=73)|(COD==86)|(COD>=90)]="OC"
  CODS[COD==0]="alive"
  d$COD2=CODS 

  d$seqnum=1 # didn't fetch from SEER*stat so stick first cancer in there
  #in mapCancsW call below benign (seqnum 60-88) will now go to misc as cancer type
  d=SEERaBomb::mapCancsW(d) # this is OK since only using cases with HM cancer diagnoses here
  d=d%>%rename(cancer=cancerW)
  head(d)
  # d=d%>%select(-seqnum,-COD,-who,-stage,-id,-race,-histo3,-rad,-chemo) # zap what is not needed in this paper
  d=d%>%select(-seqnum,-COD,-who,-id,-race,-histo3,-rad,-chemo) # zap what is not  needed, else 40 secs to save all cancs
  # system.time(save(d,file="~/data/seer25/s25.RData")) #15 secs to save all cancers if you want them
  head(d<-d%>%filter(cancer%in%c("CML"))%>%select(-cancer)) # else save only CML data
  system.time(save(d,file=file.path(seerHome,outFile))) #0.03 secs  33,541 CML cases
}
 