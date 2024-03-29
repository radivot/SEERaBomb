mapCOD2<-function(D){
  COD=D$COD #start with vec of integers. Map to a vec of Strings
  CODS=rep("unknown",dim(D)[1]) #set default to "unknown" type of death
  CODS[COD==0]="alive"
  CODS[(COD>=1)&(COD<=10)]="oral"
  CODS[COD==11]="esophagus"
  CODS[COD==12]="stomach"
  CODS[COD==13]="intestine"
  CODS[COD==14]="colon"
  CODS[COD%in%c(24,27)]="rectal"
  CODS[COD%in%c(29,30)]="liver"
  CODS[COD%in%c(31,32)]="gallBladder"
  CODS[COD==33]="pancreas"
  CODS[COD%in%c(34,35,36)]="GI"
  CODS[COD==37]="sinus"
  CODS[COD==38]="larynx"
  CODS[COD==39]="lung"
  CODS[COD%in%c(40,41,43)]="chest"
  CODS[COD==42]="bone"
  CODS[COD==44]="melanoma"
  CODS[COD==45]="skin"
  CODS[COD==46]="breast"
  CODS[COD==47]="cervix"
  CODS[COD%in%c(48,49)]="uterus"
  CODS[COD==50]="ovary"
  CODS[COD%in%c(51,53)]="femGen"
  CODS[COD==54]="prostate"
  CODS[COD==55]="testes"
  CODS[COD%in%c(56,57)]="maleGen"
  CODS[COD==58]="bladder"
  CODS[COD%in%c(59,60,61)]="renal"
  CODS[COD==62]="eye"
  CODS[COD==90]="brain"
  CODS[COD==65]="thyroid"
  CODS[COD==66]="thymus"
  CODS[COD==67]="HL"
  CODS[COD==70]="NHL"
  CODS[COD==73]="MM"
  CODS[COD==74]="ALL"
  CODS[COD==75]="CLL"
  CODS[COD%in%c(76,83,89)]="OL" 
  CODS[COD%in%c(77,80)]="AML"
  CODS[COD==78]="CML"
  CODS[COD==85]="subleu" 
  CODS[COD%in%c(86)]="otherMalig" #Miscellaneous Malignant Cancer;
  CODS[COD==130]="IS"
  CODS[COD==38000]="benign"
  CODS[COD%in%c(133,136)]="TBsyph"
  CODS[COD==142]="sepsis"
  CODS[COD==145]="otherInfect"
  CODS[COD==148]="diabetes"
  CODS[COD==151]="alzheimers"
  CODS[COD==154]="heart"
  CODS[COD==157]="hypertension"
  CODS[COD==160]="cerebroVasc"
  CODS[COD==163]="athero"
  CODS[COD==166]="aneurysm"
  CODS[COD==169]="otherVasc"
  CODS[COD==172]="pneumonia"
  CODS[COD==175]="COPD"
  CODS[COD==178]="ulcer"
  CODS[COD==181]="livFail"
  CODS[COD==184]="renFail"
  CODS[COD%in%c(187,193)]="birthing"
  CODS[COD==190]="congenDef"
  CODS[COD==196]="illDef"  #Symptoms, Signs and Ill-Defined Conditions 
  CODS[COD==199]="accidents"
  CODS[COD==202]="suicide"
  CODS[COD==205]="homocide"
  CODS[COD==208]="other"  
  # CODS[COD==24000]="HnN"  # lost Head and Neck cancer just like in Cancer definitions
  CODS[COD==252]="unknown"
  D$CODS=as.factor(CODS)
  D
}  


# [663] "  value COD_to_site_recodef"                                                              
# [664] "    0 = \"Alive\""                                                                        
# [665] "    1 = \"Lip\""                                                                          
# [666] "    2 = \"Tongue\""                                                                       
# [667] "    3 = \"Salivary Gland\""                                                               
# [668] "    4 = \"Floor of Mouth\""                                                               
# [669] "    5 = \"Gum and Other Mouth\""                                                          
# [670] "    6 = \"Nasopharynx\""                                                                  
# [671] "    7 = \"Tonsil\""                                                                       
# [672] "    8 = \"Oropharynx\""                                                                   
# [673] "    9 = \"Hypopharynx\""                                                                  
# [674] "    10 = \"Other Oral Cavity and Pharynx\""                                               
# [675] "    11 = \"Esophagus\""                                                                   
# [676] "    12 = \"Stomach\""                                                                     
# [677] "    13 = \"Small Intestine\""                                                             
# [678] "    14 = \"Colon excluding Rectum\""                                                      
# [679] "    24 = \"Rectum and Rectosigmoid Junction\""                                            
# [680] "    27 = \"Anus, Anal Canal and Anorectum\""                                              
# [681] "    29 = \"Liver\""                                                                       
# [682] "    30 = \"Intrahepatic Bile Duct\""                                                      
# [683] "    31 = \"Gallbladder\""                                                                 
# [684] "    32 = \"Other Biliary\""                                                               
# [685] "    33 = \"Pancreas\""                                                                    
# [686] "    34 = \"Retroperitoneum\""                                                             
# [687] "    35 = \"Peritoneum, Omentum and Mesentery\""                                           
# [688] "    36 = \"Other Digestive Organs\""                                                      
# [689] "    37 = \"Nose, Nasal Cavity and Middle Ear\""                                           
# [690] "    38 = \"Larynx\""                                                                      
# [691] "    39 = \"Lung and Bronchus\""                                                           
# [692] "    40 = \"Pleura\""                                                                      
# [693] "    41 = \"Trachea, Mediastinum and Other Respiratory Organs\""                           
# [694] "    42 = \"Bones and Joints\""                                                            
# [695] "    43 = \"Soft Tissue including Heart\""                                                 
# [696] "    44 = \"Melanoma of the Skin\""                                                        
# [697] "    45 = \"Non-Melanoma Skin\""                                                           
# [698] "    46 = \"Breast\""                                                                      
# [699] "    47 = \"Cervix Uteri\""                                                                
# [700] "    48 = \"Corpus Uteri\""                                                                
# [701] "    49 = \"Uterus, NOS\""                                                                 
# [702] "    50 = \"Ovary\""                                                                       
# [703] "    51 = \"Vagina\""                                                                      
# [704] "    52 = \"Vulva\""                                                                       
# [705] "    53 = \"Other Female Genital Organs\""                                                 
# [706] "    54 = \"Prostate\""                                                                    
# [707] "    55 = \"Testis\""                                                                      
# [708] "    56 = \"Penis\""                                                                       
# [709] "    57 = \"Other Male Genital Organs\""                                                   
# [710] "    58 = \"Urinary Bladder\""                                                             
# [711] "    59 = \"Kidney and Renal Pelvis\""                                                     
# [712] "    60 = \"Ureter\""                                                                      
# [713] "    61 = \"Other Urinary Organs\""                                                        
# [714] "    62 = \"Eye and Orbit\""                                                               
# [715] "    90 = \"Brain and Other Nervous System\""                                              
# [716] "    65 = \"Thyroid\""                                                                     
# [717] "    66 = \"Other Endocrine including Thymus\""                                            
# [718] "    67 = \"Hodgkin Lymphoma\""                                                            
# [719] "    70 = \"Non-Hodgkin Lymphoma\""                                                        
# [720] "    73 = \"Myeloma\""                                                                     
# [721] "    74 = \"Acute Lymphocytic Leukemia\""                                                  
# [722] "    75 = \"Chronic Lymphocytic Leukemia\""                                                
# [723] "    76 = \"Other Lymphocytic Leukemia\""                                                  
# [724] "    77 = \"Acute Myeloid Leukemia\""                                                      
# [725] "    80 = \"Acute Monocytic Leukemia\""                                                    
# [726] "    78 = \"Chronic Myeloid Leukemia\""                                                    
# [727] "    89 = \"Other Myeloid/Monocytic Leukemia\""                                            
# [728] "    83 = \"Other Acute Leukemia\""                                                        
# [729] "    85 = \"Aleukemic, Subleukemic and NOS\""                                              
# [730] "    86 = \"Miscellaneous Malignant Cancer\""                                              
# [731] "    130 = \"In situ, benign or unknown behavior neoplasm\""                               
# [732] "    133 = \"Tuberculosis\""                                                               
# [733] "    136 = \"Syphilis\""                                                                   
# [734] "    142 = \"Septicemia\""                                                                 
# [735] "    145 = \"Other Infectious and Parasitic Diseases including HIV\""                      
# [736] "    148 = \"Diabetes Mellitus\""                                                          
# [737] "    151 = \"Alzheimers (ICD-9 and 10 only)\""                                             
# [738] "    154 = \"Diseases of Heart\""                                                          
# [739] "    157 = \"Hypertension without Heart Disease\""                                         
# [740] "    160 = \"Cerebrovascular Diseases\""                                                   
# [741] "    163 = \"Atherosclerosis\""                                                            
# [742] "    166 = \"Aortic Aneurysm and Dissection\""                                             
# [743] "    169 = \"Other Diseases of Arteries, Arterioles, Capillaries\""                        
# [744] "    172 = \"Pneumonia and Influenza\""                                                    
# [745] "    175 = \"Chronic Obstructive Pulmonary Disease and Allied Cond\""                      
# [746] "    178 = \"Stomach and Duodenal Ulcers\""                                                
# [747] "    181 = \"Chronic Liver Disease and Cirrhosis\""                                        
# [748] "    184 = \"Nephritis, Nephrotic Syndrome and Nephrosis\""                                
# [749] "    187 = \"Complications of Pregnancy, Childbirth, Puerperium\""                         
# [750] "    190 = \"Congenital Anomalies\""                                                       
# [751] "    193 = \"Certain Conditions Originating in Perinatal Period\""                         
# [752] "    196 = \"Symptoms, Signs and Ill-Defined Conditions\""                                 
# [753] "    199 = \"Accidents and Adverse Effects\""                                              
# [754] "    202 = \"Suicide and Self-Inflicted Injury\""                                          
# [755] "    205 = \"Homicide and Legal Intervention\""                                            
# [756] "    208 = \"Other Cause of Death\""                                                       
# [757] "    252 = \"State DC not available or state DC available but no COD\""                    
# [758] "    ;"  