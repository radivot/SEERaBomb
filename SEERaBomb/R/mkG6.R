mkG6<-function(seerHome="~/data/seer26",
               inFile="seerMrt.RData",
               outFile="G6.RData"){
  # Use mkSEERmrt() to make inFile 
  # seerHome="~/data/seer26"
  # inFile="seerMrt.RData"
  # outFile="G6.RData"
  # require(dplyr)
  # require(forcats)
  # require(mgcv)
  # require(purrr)
  # require(rgl)
  # options(rgl.useNULL = TRUE)
  # options(rgl.printRglwidget = TRUE)
  d=COD=num=denom=year=sex=Ages=age=ageMid=a20=rate=rate2=W=NULL
  # d=COD=num=denom=year=sex=Ages=age=rate=W=NULL
  load(file.path(seerHome,inFile)) # get d with dims 336k × 7
  (d=d|>select(-rate,-rate2,-a20,-ageMid)) #drop rates since pois reg as in mkG2
  (d=d|>relocate(c(denom,num), .after = last_col())) # get num ready to sum over CODs grouped together
  # (d=d%>%select(COD,year,sex,age,denom,num)%>%arrange(COD,year,sex,age))
  # dummy x to see how total of 120M deaths breaks down, and how 1M leukemic deaths distribute
  x=d|>filter(sex=="Both")|>group_by(COD)%>%summarise(num=sum(num),.groups="drop") #shows 1M breakdown as LL=71,ML=75 & OL=80
  # warning: mortality data COD defs below need to be synced up with reg data COD defs in mkCML()              25%  50%   25%
  L=NULL
  tits=NULL
  
  dl=d|>filter(COD==70)|>mutate(COD="LC")
  (L[["LC"]]=dl)
  tits["LC"]="Leukemic Cause"

  (dc=d|>filter(COD%in%c(2:69,83:84))|>mutate(COD="NLC")) # 84 = benign included, 83 unknown primary but also MF et al
  (L[["NLC"]]=dc|>group_by(COD,sex,year,age,denom)|>summarize(num=sum(num),.groups="drop")) #210k rows => 3k rows
  tits["NLC"]="Non Leukemic Cancer"
  
  (di=d|>filter(COD%in%c(85:89,98,110))|>mutate(COD="IN")) #INfections TB, syph, hiv, septicemia, parasites 98=flu, 110=covid
  (L[["IN"]]=di|>group_by(COD,sex,year,age,denom)|>summarize(num=sum(num),.groups="drop")) # 21k => 3k
  tits["IN"]="Infection"
  
  (dv=d|>filter(COD%in%c(92:97))|>mutate(COD="CV"))
  #92=heart disease, 93=#Hypertension without Heart Disease, 94=Cerebrovascular Diseases 
  # 95 = Atherosclerosis; 96 = Aortic Aneurysm 97 = OD 
  (L[["CV"]]=dv|>group_by(COD,sex,year,age,denom)|>summarize(num=sum(num),.groups="drop")) # 18k to 3k
  tits["CV"]="Cardio and Vascular"
  
  (dk=d|>filter(COD%in%c(90,102))|>mutate(COD="DK"))  #90=DM,  # 102 = kidney disease
  (L[["DK"]]=dk|>group_by(COD,sex,year,age,denom)|>summarize(num=sum(num),.groups="drop"))
  tits["DK"]="DM & Kidney Disease"
  
  dash=d|>filter(COD%in%c(107:109))|>mutate(COD="ASH") #accidents, suicides and homocides
  (L[["ASH"]]=dash|>group_by(COD,sex,year,age,denom)|>summarize(num=sum(num),.groups="drop"))
  tits["ASH"]="Accidents, Suicides and Homocides"
  
  do=d|>filter(COD%in%c(91,99:101,103:106,111))|>mutate(COD="YOC") #yet other causes
  # 91=Alzheimers,99 = COPD, 100= stomach ulcers, 103:105 = Complications of birth, congenital, perinatal conditions 
  # 101= liver disease
  #add perinatal and ulcers to ill-defined conditions and other causes of death 
  # do=d%>%filter(COD%in%c(106,111)) #ill-defined conditions and other causes of death 
  (L[["YOC"]]=do%>%group_by(COD,year,sex,age,denom)%>%summarize(num=sum(num),.groups="drop"))
  tits["YOC"]="Yet Other Causes"
  (nms=names(L))
  G=vector(mode="list",length=length(L))
  names(G)<-nms
  for (i in nms) {
    # i="LC"
    print(i) 
    D=L[[i]]
    # (D=D%>%filter(age<90,age>40))
    # (D=D%>%filter(age<90,age>25))
    (D=D%>%filter(age>20))
    D$sex=as_factor(D$sex)
    (Df=D%>%filter(year<2020)) # data for fitting
    print(summary(G[[i]]<-gam(num ~ sex+s(age,year,by=sex)+ti(age,year)+offset(log(denom)),family=poisson(),data=Df)))
    # D$E=exp(predict(G[[i]],D))
    # head(D<-D%>%mutate(Eincid=E/denom))
    # head(D<-D%>%mutate(incid=num/denom))
    # head(D<-D%>%mutate(incid=if_else(incid==0,0.0001,incid)))
    # with(D,plot3d(year,age,log10(incid),col=ifelse(sex=="Female","red","blue"),
    #               xlab="",ylab="Age",zlab="log10(M)",alpha=1,size=4))
    # (Ages=seq(min(D$age),max(D$age)))
    # (Years=seq(min(D$year),max(D$year)))
    # head(nD<-expand.grid(Ages,Years))
    # names(nD)<-c("age","year")
    # nD$denom=1
    # nDf=nD%>%mutate(sex="Female")
    # nDm=nD%>%mutate(sex="Male")
    # nD=bind_rows(nDf,nDm)
    # head(nD)
    # dim(nD)  # = 99*35
    # nD$E=exp(predict(G[[i]],nD))
    # M=reshape2::acast(nD%>%filter(sex=="Female")%>%select(year,age,E), year~age, value.var="E")
    # surface3d(Years,Ages,log10(M),col="red",alpha=0.5) # M for Matrix
    # M=reshape2::acast(nD%>%filter(sex=="Male")%>%select(year,age,E), year~age, value.var="E")
    # surface3d(Years,Ages,log10(M),col="blue",alpha=0.5)
    # light3d(theta = 0, phi = 75) 
    # bgplot3d({
    #   plot.new()
    #   title(main = paste0(i), line = 0,cex.main=1)
    # })
    # readline(prompt=paste("Plotting",i,"Press [enter] to continue, Ctrl-C to exit"))#uncomment for snapshots
  }
  save(G,L,tits,file=file.path(seerHome,outFile)) 
}
 

# US mort defs in the sas file
# 0 = "All Causes of Death"
# 1 = "  All Malignant Cancers"   # all cancers
# 2 = "    Oral Cavity and Pharynx"
# 3 = "      Lip"
# 4 = "      Tongue"
# 5 = "      Salivary Gland"
# 6 = "      Floor of Mouth"
# 7 = "      Gum and Other Mouth"
# 8 = "      Nasopharynx"
# 9 = "      Tonsil"
# 10 = "      Oropharynx"
# 11 = "      Hypopharynx"
# 12 = "      Other Oral Cavity and Pharynx"
# 13 = "    Digestive System"
# 14 = "      Esophagus"
# 15 = "      Stomach"
# 16 = "      Small Intestine"
# 17 = "      Colon and Rectum"
# 18 = "        Colon excluding Rectum"
# 19 = "        Rectum and Rectosigmoid Junction"
# 20 = "      Anus, Anal Canal and Anorectum"
# 21 = "      Liver and Intrahepatic Bile Duct"
# 22 = "        Liver"
# 23 = "        Intrahepatic Bile Duct"
# 24 = "      Gallbladder"
# 25 = "      Other Biliary"
# 26 = "      Pancreas"
# 27 = "      Retroperitoneum"
# 28 = "      Peritoneum, Omentum and Mesentery"
# 29 = "      Other Digestive Organs"
# 30 = "    Respiratory System"
# 31 = "      Nose, Nasal Cavity and Middle Ear"
# 32 = "      Larynx"
# 33 = "      Lung and Bronchus"
# 34 = "      Pleura"
# 35 = "      Trachea, Mediastinum and Other Respiratory Organs"
# 36 = "    Bones and Joints"
# 37 = "    Soft Tissue including Heart"
# 38 = "    Skin"
# 39 = "      Melanoma of the Skin"
# 40 = "      Non-Melanoma Skin"
# 41 = "    Breast"
# 42 = "    Female Genital System"
# 43 = "      Cervix Uteri"
# 44 = "      Corpus and Uterus, NOS"
# 45 = "        Corpus Uteri"
# 46 = "        Uterus, NOS"
# 47 = "      Ovary"
# 48 = "      Vagina"
# 49 = "      Vulva"
# 50 = "      Other Female Genital Organs"
# 51 = "    Male Genital System"
# 52 = "      Prostate"
# 53 = "      Testis"
# 54 = "      Penis"
# 55 = "      Other Male Genital Organs"
# 56 = "    Urinary System"
# 57 = "      Urinary Bladder"
# 58 = "      Kidney and Renal Pelvis"
# 59 = "      Ureter"
# 60 = "      Other Urinary Organs"
# 61 = "    Eye and Orbit"
# 62 = "    Brain and Other Nervous System"
# 63 = "    Endocrine System"
# 64 = "      Thyroid"
# 65 = "      Other Endocrine including Thymus"
# 66 = "    Lymphoma"
# 67 = "      Hodgkin Lymphoma"
# 68 = "      Non-Hodgkin Lymphoma"
# 69 = "    Myeloma"
# 70 = "    Leukemia"
# 71 = "      Lymphocytic Leukemia"
# 72 = "        Acute Lymphocytic Leukemia"
# 73 = "        Chronic Lymphocytic Leukemia"
# 74 = "        Other Lymphocytic Leukemia"
# 75 = "      Myeloid and Monocytic Leukemia"
# 76 = "        Acute Myeloid Leukemia"
# 77 = "        Acute Monocytic Leukemia"
# 78 = "        Chronic Myeloid Leukemia"
# 79 = "        Other Myeloid/Monocytic Leukemia"
# 80 = "      Other Leukemia"
# 81 = "        Other Acute Leukemia"
# 82 = "        Aleukemic, Subleukemic and NOS"
# 83 = "    Miscellaneous Malignant Cancer"
# 84 = "  In situ, benign or unknown behavior neoplasm"
# 85 = "  Tuberculosis"
# 86 = "  Syphilis"
# 87 = "  Human Immunodeficiency Virus (HIV) (1987+)"
# 88 = "  Septicemia"
# 89 = "  Other Infectious and Parasitic Diseases"
# 90 = "  Diabetes Mellitus"
# 91 = "  Alzheimers (ICD-9 and 10 only)"
# 92 = "  Diseases of Heart"
# 93 = "  Hypertension without Heart Disease"
# 94 = "  Cerebrovascular Diseases"
# 95 = "  Atherosclerosis"
# 96 = "  Aortic Aneurysm and Dissection"
# 97 = "  Other Diseases of Arteries, Arterioles, Capillaries"
# 98 = "  Pneumonia and Influenza"
# 99 = "  Chronic Obstructive Pulmonary Disease and Allied Cond"
# 100 = "  Stomach and Duodenal Ulcers"
# 101 = "  Chronic Liver Disease and Cirrhosis"
# 102 = "  Nephritis, Nephrotic Syndrome and Nephrosis"
# 103 = "  Complications of Pregnancy, Childbirth, Puerperium"
# 104 = "  Congenital Anomalies"
# 105 = "  Certain Conditions Originating in Perinatal Period"
# 106 = "  Symptoms, Signs and Ill-Defined Conditions"
# 107 = "  Accidents and Adverse Effects"
# 108 = "  Suicide and Self-Inflicted Injury"
# 109 = "  Homicide and Legal Intervention"
# 110 = "  COVID-19 (2020+)"
# 111 = "  Other Cause of Death"
