mapCODs<-function(D){
  COD=D$COD #start with vec of integers. Map to a vec of Strings
  # CODS=rep("other",length(COD)) 
  CODS=rep("other",dim(D)[1]) #set default to "other" type of death
  # get mappings from  https://seer.cancer.gov/codrecode/1969+_d09172004/index.txt    
  CODS[COD==41000]=NA
  CODS[COD==0]="alive"
  # Use same names as in cancer incidence as much as possible
  CODS[(COD>=20010)&(COD<=20100)]="oral"
  CODS[COD==21010]="esophagus"
  CODS[COD==21020]="stomach"
  CODS[COD==21030]="intestine"
  CODS[COD==21040]="colon"
  CODS[COD%in%c(21050,21060)]="rectal"
  CODS[COD%in%c(21071,21072)]="liver"
  CODS[COD%in%c(21080,21090)]="gallBladder"
  CODS[COD==21100]="pancreas"
  CODS[COD%in%c(21110,21120)]="peritonium"
  CODS[COD==21130]="GI"
  CODS[COD%in%c(21110,21120)]="peritonium"
  CODS[COD==22010]="sinus"
  CODS[COD==22020]="larynx"
  CODS[COD==22030]="lung"
  CODS[COD==22050]="pleura"
  CODS[COD%in%c(22060,32020)]="thymus"
  CODS[COD==23000]="bone"
  CODS[COD==24000]="HnN"
  CODS[COD==25010]="melanoma"
  CODS[COD==25020]="skin"
  CODS[COD==26000]="breast"
  CODS[COD==27010]="cervix"
  CODS[COD%in%c(27020,27030)]="uterus"
  CODS[COD==27040]="ovary"
  CODS[COD%in%c(27050,27060,27070)]="femGen"
  CODS[COD==28010]="prostate"
  CODS[COD==28020]="testes"
  CODS[COD%in%c(28030,28040)]="maleGen"
  CODS[COD==29010]="bladder"
  CODS[COD%in%c(29020,29030,29040)]="renal"
  CODS[COD==30000]="eye"
  CODS[COD==31010]="brain"
  CODS[COD==32010]="thyroid"
  CODS[COD==32010]="HL"
  CODS[COD==33040]="NHL"
  CODS[COD==34000]="MM"
  CODS[COD==35011]="ALL"
  CODS[COD==35012]="CLL"
  CODS[COD%in%c(35021,35031)]="AML"
  CODS[COD==35022]="CML"
  CODS[COD%in%c(35013,35023,35041,35043)]="OL"
  CODS[COD%in%c(36010,36020,37000)]="otherMalig" #Mesothelioma ;Kaposi Sarcoma ;Miscellaneous Malignant Cancer;
  CODS[COD==38000]="benign"
  ### these didn't map
  # AMLti, APL breastCIS, cervixCIS CMML femGenCIS, giCIS, guCIS, HCL,  LGL, 
  # maleGenCIS, MDS, MPN, nerves, other, otherCIS respCIS, skinCIS, SLL, unknown   
  CODS[COD%in%c(50000,50010,50030,50040)]="infection"
  CODS[COD==50050]="diabetes"
  CODS[COD==50051]="alzheimers"
  CODS[COD==50060]="heart"
  CODS[COD==50070]="hypertension"
  CODS[COD==50080]="cerebroVasc"
  CODS[COD==50090]="athero"
  CODS[COD==50100]="aneurysm"
  CODS[COD==50110]="otherVasc"
  CODS[COD==50120]="pneumonia"
  CODS[COD==50130]="COPD"
  CODS[COD==50140]="ulcer"
  CODS[COD==50150]="livFail"
  CODS[COD==50160]="renFail"
  CODS[COD%in%c(50170,50190)]="birthing"
  CODS[COD==50180]="congenDef"
  CODS[COD%in%c(50200,50300)]="other"  #Symptoms, Signs and Ill-Defined Conditions +Other 
  CODS[COD==50210]="accidents"
  CODS[COD==50220]="suicide"
  CODS[COD==50230]="homocide"
  # CODS=as.factor(CODS)
  # CODS
  D$CODS=as.factor(CODS)
  D
}  

# > table(d$COD,useNA="always")
# 0         20010   20020   20030   20040   20050   20060   20070   20080   20090   20100   21010   21020   21030   21040 
# 4291579     479   15586    4913    1695    9289    5500    4695    4609    3322   10469   84044   99606    8183  314870 
# 21050     21060   21071   21072   21080   21090   21100   21110   21120   21130   22010   22020   22030   22050   22060 
# 61830      4338   84712   22767   14739   11921  217308    1983    4276    5987    3251   25226  959251    2617    2076 
# 23000     24000   25010   25020   26000   27010   27020   27030   27040   27050   27060   27070   28010   28020   28030 
# 8543      26078   51929   12574  263225   26121   22419   26360   92240    2820    5268    2599  172641    2762    1374 
#   28040   29010   29020   29030   29040   30000   31010   32010   32020   33010   33040   34000   35011   35012   35013 
#     315   94125   76870    2717    2522    1812   86342    9366    5837    9416  133827   66092   10523   23907    3015 
#   35021   35022   35023   35031   35041   35043   37000   38000   41000   50000   50010   50030   50040   50050   50051 
#   55564   10102    4613    1246   15650   17320  259226   50141   83211     843      35   31119   47310   52261   50230 
#   50060   50070   50080   50090   50100   50110   50120   50130   50140   50150   50160   50170   50180   50190   50200 
#  629721   21364  133618   14507   12032    8797   72133  134635    5729   22419   37569     342    2458     125   21923 
#   50210   50220   50230   50300    <NA> 
#   46647   14579    1705  279381       0 
