mapCancsP<-function(D){
  site=D$site
  #   histo2=D$histo2
  histo3=D$histo3
  cancer=rep("other",dim(D)[1])
  cancer[(site==809)]="unknown"
  cancer[(site>=0)&(site<150)]="oral"
  cancer[(site>=150)&(site<=159)]="esophagus"
  cancer[(site>=160)&(site<=169)]="stomach"
  cancer[(site>=170)&(site<=179)]="intestine"
  cancer[(site>=180)&(site<=189)]="colon"
  cancer[(site>=190)&(site<=219)]="rectal"
  cancer[(site>=220)&(site<=229)]="liver"
  cancer[(site>=230)&(site<=239)]="gallBladder"
  cancer[(site>=240)&(site<=259)]="pancreas"
  cancer[(site>=260)&(site<=269)]="GI"
  cancer[(site>=300)&(site<=319)]="sinus"
  cancer[(site>=320)&(site<=329)]="larynx"
  cancer[(site>=330)&(site<=349)]="lung"
  cancer[(site>=370)&(site<=379)]="thymus"
  cancer[(site>=380)&(site<=399)]="pleura"
  cancer[(site>=400)&(site<=419)]="bone"
  cancer[(site>=420)&(site<=429)]="heme"
  cancer[(site>=440)&(site<=449)]="skin"
  cancer[(site>=470)&(site<=479)]="nerves"
  cancer[(site>=480)&(site<=489)]="retro"
  cancer[(site>=490)&(site<=499)]="connect"
  cancer[(site>=500)&(site<=509)]="breast"
  cancer[(site>=510)&(site<=529)]="femGen"
  cancer[(site>=530)&(site<=539)]="cervix"
  cancer[(site>=540)&(site<=549)]="uterus"
  cancer[(site>=560)&(site<=569)]="ovary"
  cancer[(site>=570)&(site<=589)]="femGen"
  cancer[(site>=600)&(site<=609)]="maleGen"
  cancer[site==619]="prostate"
  cancer[(site>=620)&(site<=629)]="testes"
  cancer[(site>=630)&(site<=639)]="maleGen"
  cancer[(site>=640)&(site<=659)]="renal"
  cancer[(site>=660)&(site<=689)]="bladder"
  cancer[(site>=690)&(site<=699)]="eye"
  cancer[(site>=700)&(site<=729)]="brain"
  cancer[site==739]="thyroid"
  cancer[(site>=740)&(site<=759)]="glands"
  cancer[(site>=760)&(site<=809)]="NOS"
  
# 
#   cancer[(site>=1710)&(site<=1719)]="HnN" # went to oral?
#   cancer[(site>=1720)&(site<=1729)]="melanoma"
#   cancer[site==1991]="otherMalig"
  
  # clean things with histO3 codes that trump the heme ICD9 codes above
  cancer[(histo3>=9590)&(histo3<9600)]="NHL" 
  # cancer[(histo3>=9650)&(histo3<9670)]="hodgkin" 
  cancer[(histo3>=9650)&(histo3<9670)]="HL" 
  cancer[(histo3>=9670)&(histo3<9730)]="NHL" 
  cancer[(histo3>=9730)&(histo3<9735)]="MM" 
  cancer[(histo3>=9735)&(histo3<9740)]="NHL" 
  cancer[(histo3>=9740)&(histo3<=9742)]="MPN" #"mastocytosis" 
  cancer[(histo3>=9743)&(histo3<9760)]="MPN" # assume histiocytosis is more like MPN than NHL
  #   cancer[(histo3==9751)]="MPN" #"LCH" lagerhan cell histiocytes are APCs = macrophage like, guessing MPN-like
  cancer[(histo3>=9760)&(histo3<=9770)]="MM"  # outside of below there are ~20 cases of these
  #   this cancer[(ICD9>=2730)&(ICD9<2739)]="globinemia" yielded 5080 cases of 9761 and 9762
  
  
  cancer[(histo3>=9800)&(histo3<9810)]="OL" # takes back 60 AMLs from ICD9 in 9808 and 9809
  cancer[histo3==9948]="OL"  # aggressive NK leukemia lumped in here
  #   cancer[(histo3==9812)|(histo3==9806)]="ALLba" #ALL with BCR-ABL (110 cases) 2010-12 + 12 mixed lineage 2011-12
  cancer[(histo3>=9810)&(histo3<9840)]="ALL" # take some OL back to ALL
  cancer[(histo3==9831)&(D$yrdx>2009)]="LGL" # 9831 is cleanly LGL only starting in 2010
  cancer[(histo3==9823)]="CLL" # pull out the CLLs
  # cancer[(histo3==9670)]="SLL" # SLL has different risk time course, so better not merge with CLL.
  cancer[(histo3==9670)]="CLL" #more 2nd SLL since earlier DX of CLL=> more likely SLL, so better to pool
  # Dutch study placed 9800 and 9820 also in CLL, but incidence age responses are clearly different.
  cancer[(histo3>=9840)&(histo3<9940)]="AML" # take some OL back to AML, includes next two lines
  #   cancer[(histo3==9910)]="AML" #AMKL 
  #   cancer[(histo3==9930)]="AML" #myeloid sarcoma (blasts forming tumor outside of marrow ... advanced AML)
  cancer[histo3%in%c(9863,9875)]="CML"  # take back CMLs
  cancer[(histo3==9866)]="APL" # andmake APL exclusive
  cancer[histo3%in%c(9865,9869,9871,9896,9897,9911)]="AMLti"  # AML by tranlocation or inversion
  #  t(6,9),inv(3),inv(16),t(8,21),t(9,11),t(1,22)
  cancer[(histo3>9979)&(histo3<9990)]="MDS" ##!!!!! tMDS=9987 got mapped in with tAML=9920 starting in 2010
  #   cancer[(histo3==9987)]="tMDS" ##!!! so we have to pull both out and correct for this
  #   cancer[(histo3==9920)]="tAML" 
  #   cancer[(histo3==9982)]="RARS" # take out to look for correlations with CLL via SF3B1 
  #   cancer[(histo3==9986)]="MDSdel5q" # take out to look for extra radiation induction (skip: confounded by tMDS)
  #9980=RA; 9981=nothing, 9983=RAEB, 9984=RAEB-T transformation (also stopped in 2010),9985=RCMD, 9989=NOS
  cancer[(histo3==9940)]="HCL"  #hairy cell leukemia was getting into NHL (note: HCL in 20's goes to 9591=NHL)
  cancer[(histo3==9945)]="CMML" 
#   cancer[(histo3==9960)]="MDS" #"CMPD" #this got remapped to mdsMPN = 9975 in 2010
#   cancer[(histo3==9975)]="MDS"  #"mdsMPN": guessing this is CMML-like, and more MDS-like than MPN-like
  cancer[(histo3==9960)]="MPN" #switch to MPN to keep MDS clean 
  cancer[(histo3==9975)]="MPN" #same here, better keep MDS clean
  cancer[(histo3==9946)]="MPN" #"jCMML" these come out of 205.1/CML
  cancer[(histo3==9950)]="MPN" #"PV"
  cancer[(histo3==9961)]="MPN" #"PMF"
  cancer[(histo3==9962)]="MPN" #"ET"
  cancer[(histo3==9963)]="MPN" #"CNL"
  cancer[(histo3==9964)]="MPN" #"CEL"
  cancer[(histo3>=9965)&(histo3<=9967)]="MPN" # GFR mutatants 
  cancer[(histo3>=9970)&(histo3<=9971)]="NHL" # ICD9 put it mostly there, so sweep stray 1s into it also. 
  cancer[(histo3==9876)]="MPN" #"aCML"
  cancer[histo3==9140]="KS" # overwrite mostly skin as Kaposi Sarcoma (KS)
  cancer[(D$seqnum>=60)&(D$seqnum<=88)]="benign" # 88 is benign but unknown sequence
  ## most of below fall into benign, and most started in 2004.
  ##  Bottomline: Bucket to remove since my codes don't handle such seqnums.
  ##  Complications of handling them include: if I map 60 to 0 and 61 to 1, trouble may come in one caseID havinf 2 seqnum=0 or 1
  ##  rows. I'll leave figuring out how to handle this to someone with real interests in brain tumors. 
  #   cancer[(histo3==9530)]="meningioma" #supposedly malignant, but seqnums >59 confuse this.
  # #   cancer[(histo3>=9531)&(histo3<=9539)]="meningioma" #benigns, mix in with mal or comment to pool with unknown
  #   cancer[(histo3==9560)]="schwannoma"
  #   cancer[(histo3==8272)]="pituitary"
  D$cancer=as.factor(cancer)
  D
}  
