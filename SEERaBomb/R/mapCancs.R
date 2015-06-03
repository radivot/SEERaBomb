mapCancs<-function(D){
  ICD9=D$ICD9
  #   histo2=D$histo2
  histo3=D$histo3
  cancer=rep("other",dim(D)[1])
  cancer[(ICD9==9999)]="unknown"
  #   cancer[(ICD9<1450)]="l140"
  cancer[(ICD9>=2300)&(ICD9<2310)]="giCIS"
  cancer[(ICD9>=2310)&(ICD9<2320)]="respCIS"
  cancer[(ICD9>=2320)&(ICD9<2330)]="skinCIS"
  cancer[(ICD9==2330)]="breastCIS"
  cancer[(ICD9==2331)]="cervixCIS"
  cancer[(ICD9>=2332)&(ICD9<=2333)]="femGenCIS"
  cancer[(ICD9>=2334)&(ICD9<=2336)]="maleGenCIS"
  cancer[(ICD9>=2337)&(ICD9<2340)]="guCIS"
  cancer[(ICD9>=2340)&(ICD9<2349)]="otherCIS"
  cancer[(ICD9>=1400)&(ICD9<1500)]="oral"
  cancer[(ICD9>=1500)&(ICD9<=1509)]="esophagus"
  cancer[(ICD9>=1510)&(ICD9<=1519)]="stomach"
  cancer[(ICD9>=1520)&(ICD9<=1529)]="intestine"
  cancer[(ICD9>=1530)&(ICD9<=1539)]="colon"
  cancer[(ICD9>=1540)&(ICD9<=1549)]="rectal"
  cancer[(ICD9>=1550)&(ICD9<=1559)]="liver"
  cancer[(ICD9>=1560)&(ICD9<=1569)]="gallBladder"
  cancer[(ICD9>=1570)&(ICD9<=1579)]="pancreas"
  cancer[(ICD9>=1580)&(ICD9<=1589)]="peritonium"
  cancer[(ICD9>=1590)&(ICD9<=1599)]="GI"
  cancer[(ICD9>=1600)&(ICD9<=1609)]="sinus"
  cancer[(ICD9>=1610)&(ICD9<=1619)]="larynx"
  cancer[(ICD9>=1620)&(ICD9<=1629)]="lung"
  cancer[(ICD9==1639)]="pleura"
  cancer[(ICD9>=1640)&(ICD9<=1649)]="thymus"
  cancer[(ICD9>=1700)&(ICD9<=1709)]="bone"
  cancer[(ICD9>=1710)&(ICD9<=1719)]="HnN"
  cancer[(ICD9>=1720)&(ICD9<=1729)]="melanoma"
  cancer[(ICD9>=1730)&(ICD9<=1739)]="skin"
  cancer[(ICD9>=1740)&(ICD9<=1749)]="breast"
  cancer[(ICD9==175)|((ICD9>=1750)&(ICD9<=1759))]="breast"
  cancer[ICD9==179]="uterus"
  cancer[(ICD9>=1800)&(ICD9<=1809)]="cervix"
  #   cancer[ICD9==181]="pla"
  cancer[(ICD9>=1820)&(ICD9<=1829)]="uterus"
  cancer[(ICD9>=1830)&(ICD9<=1839)]="ovary"
  cancer[(ICD9==2362)]="ovary"
  cancer[(ICD9>=1840)&(ICD9<=1849)]="femGen"
  cancer[ICD9==185]="prostate"
  cancer[(ICD9>=1860)&(ICD9<=1869)]="testes"
  cancer[(ICD9>=1870)&(ICD9<=1879)]="maleGen"
  cancer[(ICD9>=1880)&(ICD9<=1889)]="bladder"
  cancer[(ICD9>=1890)&(ICD9<=1899)]="renal"
  cancer[(ICD9>=1900)&(ICD9<=1909)]="eye"
  cancer[(ICD9>=1910)&(ICD9<=1919)]="brain"
  cancer[(ICD9>=1920)&(ICD9<=1929)]="nerves"
  cancer[ICD9==193]="thyroid"
  cancer[ICD9==1991]="otherMalig"
  cancer[(ICD9>=2000)&(ICD9<2010)]="NHL"
  cancer[(ICD9>=2010)&(ICD9<2020)]="hodgkin"
  cancer[(ICD9>=2020)&(ICD9<2030)]="NHL"
  cancer[(ICD9>=2030)&(ICD9<2040)]="MM"
  cancer[(ICD9>=2040)&(ICD9<2050)]="ALL"
  cancer[(ICD9>=2050)&(ICD9<2080)]="AML"  # same here
  cancer[(ICD9>=2080)&(ICD9<2090)]="OL" #M0 like stuff
  cancer[(ICD9==2387)]="OL"
  cancer[(histo3>=9820)&(histo3<9840)]="ALL" # take some OL back to ALL
  cancer[histo3==9823]="CLL" # pull out the CLLs
  #   cancer[(histo3==9812)|(histo3==9806)]="ALLba" #ALL with BCR-ABL (110 cases) 2010-12 + 12 mixed lineage 2011-12
  cancer[(histo3>=9840)&(histo3<9940)]="AML" # take some OL back to AML, includes next two lines
  #   cancer[(histo3==9910)]="AML" #AMKL 
  #   cancer[(histo3==9930)]="AML" #myeloid sarcoma (blasts forming tumor outside of marrow ... advanced AML)
  cancer[histo3%in%c(9863,9875)]="CML"  # take back CMLs
  cancer[(histo3==9866)]="APL" # andmake APL exclusive
    
  cancer[(histo3>9979)&(histo3<9990)]="MDS" ##!!!!! tMDS=9987 got mapped in with tAML=9920 in 2010
  cancer[(histo3==9920)|(histo3==9987)]="tAML" ##!!! so we have to pull both out and look at them separately
  cancer[(histo3==9945)|(ICD9==2061)]="CMML" # the latter takes some out of 9860=AML M0
  cancer[(histo3==9960)]="MDS" #"CMPD" #this got remapped to mdsMPN = 9975 in 2010
  cancer[(histo3==9975)]="MDS"  #"mdsMPN": guessing this is CMML-like, and more MDS-like than MPN-like
  cancer[(histo3>=9740)&(histo3<=9742)]="MPN" #"mastocytosis" 
  cancer[(histo3==9751)]="MPN" #"LCH" lagerhan cell histiocytes are APCs = macrophage like, guessing MPN-like
  cancer[(histo3==9946)]="MPN" #"jCMML" these come out of 205.1/CML
  cancer[(histo3==9950)]="MPN" #"PV"
  cancer[(histo3==9961)]="MPN" #"PMF"
  cancer[(histo3==9962)]="MPN" #"ET"
  cancer[(histo3==9963)]="MPN" #"CNL"
  cancer[(histo3==9964)]="MPN" #"CEL"
  cancer[(histo3==9876)]="MPN" #"aCML"
  cancer[(ICD9>=2730)&(ICD9<2739)]="globinemia"
  cancer[(histo3==9530)]="meningioma"
  cancer[(histo3==9560)]="schwannoma"
  cancer[(histo3==8272)]="pituitary"
  D$cancer=as.factor(cancer)
  D
}  
