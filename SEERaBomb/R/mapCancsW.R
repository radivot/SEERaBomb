mapCancsW<-function(D){
  who=D$who
  histo3=D$histo3
  cancer=rep("misc",dim(D)[1])
  cancer[(who>=0)&(who<=10)]="oral"
  cancer[who==11]="esophagus"
  cancer[who==12]="stomach"
  cancer[(who>=13)&(who<=16)]="intestine"
  cancer[(who>=17)&(who<=23)]="colon"
  cancer[(who>=25)&(who<=27)]="rectal"
  cancer[(who>=29)&(who<=30)]="liver"
  cancer[(who>=31)&(who<=32)]="gallBladder"
  cancer[who==33]="pancreas"
  cancer[(who>=34)&(who<=36)]="GI"
  cancer[who==37]="sinus"
  cancer[who==38]="larynx"
  cancer[(who==39)|(who==41)]="lung"
  cancer[who==40]="pleura"
  cancer[who==42]="bone"
  cancer[who==43]="soft" # includes heart
  cancer[who==44]="melanoma"
  cancer[who==45]="skin"
  cancer[who==46]="breast"
  cancer[who==47]="cervix"
  cancer[(who>=48)&(who<=49)]="uterus"
  cancer[who==50]="ovary"
  cancer[(who>=51)&(who<=53)]="femGen"
  
  cancer[who==54]="prostate"
  cancer[who==55]="testes"
  cancer[(who>=56)&(who<=57)]="maleGen"
  
  cancer[who==59]="renal"
  cancer[(who==58)|(who==60)|(who==61)]="bladder"
  
  cancer[who==62]="eye"
  cancer[(who>=63)&(who<=64)]="brain"
  cancer[who==65]="thyroid"
  cancer[who==66]="thymus"
  
  ### these 3 here in who but not in prim
  cancer[who==86]="misc"
  cancer[who==87]="pleura" #meso pooled with pleura
  cancer[who==88]="KS"
  
  ##### hemes
  cancer[(who>=68)&(who<=69)]="HL"
  cancer[(who>=71)&(who<=72)]="NHL"
  cancer[who==73]="MM"
  cancer[who==74]="ALL"
  cancer[who==76]="ALL" # other acute lymphoid
  cancer[who==75]="CLL"
  cancer[who==77]="AML"
  cancer[who==80]="AML"  # "AMoL"
  cancer[who==89]="AML"  # "Other Myeloid/Monocytic Leukemia"
  cancer[who==78]="CMLw"  # see if any left behind (0 is good => not in factor at end)
  cancer[who==83]="OL"    #  other leukemia myeloid or lymphoid
  cancer[who==85]="subLeu"  # see if anything left
  
  
  # clean things with histo3 codes that trump codes above
  ###### Low numbers are mostly Lymphoid, except for a few MPN
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
  # cancer[(histo3==9812)|(histo3==9806)]="ALLba" #ALL with BCR-ABL (110 cases) 2010-12 + 12 mixed lineage 2011-12
  cancer[(histo3>=9810)&(histo3<9840)]="ALL" # take some OL back to ALL
  cancer[(histo3==9831)&(D$yrdx>2009)]="LGL" # 9831 is cleanly LGL only starting in 2010
  cancer[(histo3==9823)]="CLL" # pull out the CLLs
  # cancer[(histo3==9670)]="SLL" # SLL has different risk time course, so better not merge with CLL.
  cancer[(histo3==9670)]="CLL" #more 2nd SLL since earlier DX of CLL=> more likely SLL, so better to pool
  # Dutch study placed 9800 and 9820 also in CLL, but incidence age responses are clearly different.
  
###### Myeloid
  cancer[(histo3>=9840)&(histo3<9940)]="AML" # take some OL back to AML, includes next two lines
  #   cancer[(histo3==9910)]="AML" #AMKL 
  #   cancer[(histo3==9930)]="AML" #myeloid sarcoma (blasts forming tumor outside of marrow ... advanced AML)
  cancer[histo3%in%c(9863,9875)]="CML"  # take back CMLs
  cancer[(histo3==9866)]="APL" # andmake APL exclusive
  cancer[histo3%in%c(9865,9869,9871,9896,9897,9911)]="AMLti"  # AML by tranlocation or inversion
  #   #  t(6,9),inv(3),inv(16),t(8,21),t(9,11),t(1,22)
  cancer[(histo3>9979)&(histo3<9990)]="MDS" ##!!!!! tMDS=9987 got mapped in with tAML=9920 starting in 2010
  #   #   cancer[(histo3==9987)]="tMDS" ##!!! so we have to pull both out and correct for this
  #   #   cancer[(histo3==9920)]="tAML"
  #   #   cancer[(histo3==9982)]="RARS" # take out to look for correlations with CLL via SF3B1
  #   #   cancer[(histo3==9986)]="MDSdel5q" # take out to look for extra radiation induction (skip: confounded by tMDS)
  #   #9980=RA; 9981=nothing, 9983=RAEB, 9984=RAEB-T transformation (also stopped in 2010),9985=RCMD, 9989=NOS
  cancer[(histo3==9940)]="HCL"  #hairy cell leukemia was getting into NHL (note: HCL in 20's goes to 9591=NHL)
  cancer[(histo3==9945)]="CMML"
  #   cancer[(histo3==9960)]="MDS" #"CMPD" #this got remapped to mdsMPN = 9975 in 2010
  #   cancer[(histo3==9975)]="MDS"  #"mdsMPN": guessing this is CMML-like, and more MDS-like than MPN-like
  cancer[(histo3==9960)]="MPN" #switch to MPN to keep MDS clean
  cancer[(histo3==9975)]="MPN" #same here, better keep MDS clean
  cancer[(histo3==9946)]="MPN" #"jCMML" these come out of 205.1/CML
  # cancer[(histo3==9946)]="jCMML" #"jCMML" these come out of 205.1/CML
  # cancer[(histo3==9950)]="MPN" #"PV"
  cancer[(histo3==9950)]="PV" #"PV"
  cancer[(histo3==9961)]="MPN" #"PMF"
  # cancer[(histo3==9962)]="MPN" #"ET"
  cancer[(histo3==9962)]="ET" #"ET"
  cancer[(histo3==9963)]="MPN" #"CNL"
  cancer[(histo3==9964)]="MPN" #"CEL"
  cancer[(histo3>=9965)&(histo3<=9967)]="MPN" # GFR mutatants
  #   cancer[(histo3>=9970)&(histo3<=9971)]="NHL" # ICD9 put it mostly there, so sweep stray 1s into it also.
  cancer[(histo3==9876)]="MPN" #"aCML"
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
  D$cancerW=as.factor(cancer)
  D
}  

# SAS file mapping of SEER*stat codes to labels
# 
# 503] "  value Site_recode_ICD_O_3_WHO_2008f"                                                    
# [504] "    1 = \"Lip\""                                                                          
# [505] "    2 = \"Tongue\""                                                                       
# [506] "    3 = \"Salivary Gland\""                                                               
# [507] "    4 = \"Floor of Mouth\""                                                               
# [508] "    5 = \"Gum and Other Mouth\""                                                          
# [509] "    6 = \"Nasopharynx\""                                                                  
# [510] "    7 = \"Tonsil\""                                                                       
# [511] "    8 = \"Oropharynx\""                                                                   
# [512] "    9 = \"Hypopharynx\""                                                                  
# [513] "    10 = \"Other Oral Cavity and Pharynx\""                                               
# [514] "    11 = \"Esophagus\""                                                                   
# [515] "    12 = \"Stomach\""                                                                     
# [516] "    13 = \"Small Intestine\""                                                             
# [517] "    15 = \"Cecum\""                                                                       
# [518] "    16 = \"Appendix\""                                                                    
# [519] "    17 = \"Ascending Colon\""                                                             
# [520] "    18 = \"Hepatic Flexure\""                                                             
# [521] "    19 = \"Transverse Colon\""                                                            
# [522] "    20 = \"Splenic Flexure\""                                                             
# [523] "    21 = \"Descending Colon\""                                                            
# [524] "    22 = \"Sigmoid Colon\""                                                               
# [525] "    23 = \"Large Intestine, NOS\""                                                        
# [526] "    25 = \"Rectosigmoid Junction\""                                                       
# [527] "    26 = \"Rectum\""                                                                      
# [528] "    27 = \"Anus, Anal Canal and Anorectum\""                                              
# [529] "    29 = \"Liver\""                                                                       
# [530] "    30 = \"Intrahepatic Bile Duct\""                                                      
# [531] "    31 = \"Gallbladder\""                                                                 
# [532] "    32 = \"Other Biliary\""                                                               
# [533] "    33 = \"Pancreas\""                                                                    
# [534] "    34 = \"Retroperitoneum\""                                                             
# [535] "    35 = \"Peritoneum, Omentum and Mesentery\""                                           
# [536] "    36 = \"Other Digestive Organs\""                                                      
# [537] "    37 = \"Nose, Nasal Cavity and Middle Ear\""                                           
# [538] "    38 = \"Larynx\""                                                                      
# [539] "    39 = \"Lung and Bronchus\""                                                           
# [540] "    40 = \"Pleura\""                                                                      
# [541] "    41 = \"Trachea, Mediastinum and Other Respiratory Organs\""                           
# [542] "    42 = \"Bones and Joints\""                                                            
# [543] "    43 = \"Soft Tissue including Heart\""                                                 
# [544] "    44 = \"Melanoma of the Skin\""                                                        
# [545] "    45 = \"Other Non-Epithelial Skin\""                                                   
# [546] "    46 = \"Breast\""                                                                      
# [547] "    47 = \"Cervix Uteri\""                                                                
# [548] "    48 = \"Corpus Uteri\""                                                                
# [549] "    49 = \"Uterus, NOS\""                                                                 
# [550] "    50 = \"Ovary\""                                                                       
# [551] "    51 = \"Vagina\""                                                                      
# [552] "    52 = \"Vulva\""                                                                       
# [553] "    53 = \"Other Female Genital Organs\""                                                 
# [554] "    54 = \"Prostate\""                                                                    
# [555] "    55 = \"Testis\""                                                                      
# [556] "    56 = \"Penis\""                                                                       
# [557] "    57 = \"Other Male Genital Organs\""                                                   
# [558] "    58 = \"Urinary Bladder\""                                                             
# [559] "    59 = \"Kidney and Renal Pelvis\""                                                     
# [560] "    60 = \"Ureter\""                                                                      
# [561] "    61 = \"Other Urinary Organs\""                                                        
# [562] "    62 = \"Eye and Orbit\""                                                               
# [563] "    63 = \"Brain\""                                                                       
# [564] "    64 = \"Cranial Nerves Other Nervous System\""                                         
# [565] "    65 = \"Thyroid\""                                                                     
# [566] "    66 = \"Other Endocrine including Thymus\""                                            
# [567] "    68 = \"Hodgkin - Nodal\""                                                             
# [568] "    69 = \"Hodgkin - Extranodal\""                                                        
# [569] "    71 = \"NHL - Nodal\""                                                                 
# [570] "    72 = \"NHL - Extranodal\""                                                            
# [571] "    73 = \"Myeloma\""                                                                     
# [572] "    74 = \"Acute Lymphocytic Leukemia\""                                                  
# [573] "    75 = \"Chronic Lymphocytic Leukemia\""                                                
# [574] "    76 = \"Other Lymphocytic Leukemia\""                                                  
# [575] "    77 = \"Acute Myeloid Leukemia\""                                                      
# [576] "    80 = \"Acute Monocytic Leukemia\""                                                    
# [577] "    78 = \"Chronic Myeloid Leukemia\""                                                    
# [578] "    89 = \"Other Myeloid/Monocytic Leukemia\""                                            
# [579] "    83 = \"Other Acute Leukemia\""                                                        
# [580] "    85 = \"Aleukemic, Subleukemic and NOS\""                                              
# [581] "    87 = \"Mesothelioma\""                                                                
# [582] "    88 = \"Kaposi Sarcoma\""                                                              
# [583] "    86 = \"Miscellaneous\""                                                               
# [584] "    ;"         
# 

