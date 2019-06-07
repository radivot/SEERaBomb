mkAbomb<-function(AbombHome="~/data/abomb"){
  #   library(RSQLite)
  # gimic to get rid of unwanted notes in R CMD check
  marD=py=doseg=ovaD=tesD=uteD=g=NULL #seems to be OK with D and n coming from some global source: function names?
  year=city=un4gy=calg=ageG=DG=gdist=upy=subjects=NHL=tot=marg=marn=colD=histnos=NULL

  AbombHome=path.expand(AbombHome)
  outDB=file.path(AbombHome,"abomb.db") 
  m <- dbDriver("SQLite")
  con <- dbConnect(m, dbname = outDB)
  colName = c(
    "city","sex","un4gy","distg","agexg","ageg","doseg",
    "calg","upy","py","subjects","gdist","agex","age","year",
    "NHL","HL","MM","ALL","OLL","ALLtot","CLL","HCL","CLLtot","ATL",
    "AML","OML","AMoL","AMLtot","CML","othleuk","nonCLL","leuktot",
    "tot","D","g","n")
  added=c("t","ageG","agexG","tG","DG")
  desc=c(
    "Hiroshima=1, Nagasaki=2",                               "Male=1, Female=2",
    "Under 4 Gy of Shielded Kerma Total (G+N) indicator",    "Ground distance categories",
    "Age at exposure categories",                            "Attained age categories",     
    "Marrow dose (gamma + 10*neutron) categories",           "Calendar Time",    
    "Unadjusted person years at risk",                       "Adjusted person years at risk",
    "The number of subjects first at risk in each cell",
    "Person-year weighted mean Ground distance in meters",   "Person-year weighted mean Age at exposure in years",
    "Person-year weighted mean Attained age in years",       "Person-year weighted mean year",
    "Non Hodgkin lymphoma",
    "Hodgkin lymphoma",
    "Myeloma",
    "Acute lymphoblastic leukemia",
    "Aleukemia/subleukemic lymphoid leukemia",
    "total ALL",
    "Chronic lymphocytic leukemia",
    "Hairy cell leukemia",
    "Total CLL",
    "Adult T-cell leukemia",
    "Acute myeloid leukemia",
    "a-/sub-leukemic myeloid leukemia, or myeloid leukemia NOS",
    "Acute monocytic leukemia",
    "Total AML",
    "Chronic myeloid leukemia",
    "Other leukemia",
    "Non-CLL/non-ATL leumemia",
    "Total leukemia",
    "All events",
    "DS02 Bone Marrow dose Sv",    "DS02 Bone Marrow Gamma",    "DS02 Bone Marrow Neutron",
    "time since bombing","age Group","agex Group","t Group","Dose Group")
  nms=c(colName,added)
  df=data.frame(nms,desc)
  d=read.csv(file.path(AbombHome,"lsshempy.csv"),col.names=colName)
  d=d%>%mutate(D=D/1000,g=g/1000,n=n/1000,t=year-1945.6)  #%.%
  # head(d,2)
  d$ageG=cut(d$age,c(0,20,40,60,70,80,110))
  d$agexG=cut(d$agex,c(0,20,40,110))
  d$tG=cut(d$t,c(0,10,20,40,110)) 
  d$DG=cut(d$D,c(-1,.02,1,100),labels=c("Ctrl","<1Sv",">1Sv"))
  d$DG=factor(d$DG,levels=c(">1Sv","<1Sv","Ctrl"))#>1Sv at top
  # head(d,2)
  L=as.list(desc)
  names(L)=nms
  labelled::var_label(d) <- L
  # View(d)
  d=d%>%select(city:un4gy,calg,ageG:DG,gdist:year,D:t,py,upy,subjects,NHL:tot)
  # head(d,2)
  #     filter(doseg>1)  #this group has negative doses, which means they are unknown, so take them out. 
  #     filter(D>=0,py>0)%.% 
  #     mutate(dose=cut(D,c(0,.02,.4,10),include.lowest=TRUE,labels=c("low","med","high"))) 
  dbWriteTable(con, "heme", d,overwrite=TRUE)
  dbWriteTable(con, "hemeDesc", df,overwrite=TRUE)
  heme=as_tibble(d)
  # hemeDesc=as_tibble(df)
  
  colName = c("city", "sex", "un4gy","distg","agexg","ageg","doseg",
              "calg", "subjects", "upy", "py", "gdist", "agex", "age", "year", 
              "solid", "oralca", "lip", "tongue", "saliv", "mouth", "pharynx", 
              "digestca", "esoph", "stomach", "smallint", "colon", "rectum", 
              "liver", "gallbldr", "pancr", "othdig", "respca", "nasal", "larynx", 
              "lung", "othres", "thymus", "skinbone", "bone", "connect", "nmskin", 
              "skbasal", "sksquam", "bowens", "breast", "femgenca", "uterus", 
              "utrNOS", "cervix", "corpus", "ovary", "othfem", "malgenca", 
              "prost", "testis", "othmale", "urinca", "bladder", "kidney", 
              "renal", "othurin", "cnsca", "thyroid", "othsol", "msother", 
              "adeno", "squam", "othepi", "sarcoma", "othnonepi", "histnos", 
              "colD","colg","coln", "marD","marg","marn", "braD", "breD", "livD", "livg", "livn", "lunD", 
              "ovaD", "panD", "skeD", "skiD", "stoD", "tesD", "thyD", "blaD", "uteD","trunc", "adj")
  
  desc=c(
    "Hiroshima=1, Nagasaki=2",                               "Male=1, Female=2",
    "Under 4 Gy of Shielded Kerma Total (G+N) indicator",    "Ground distance categories",
    "Age at exposure categories",                            "Attained age categories",     
    "Marrow dose (gamma + 10*neutron) categories",           "Calendar Time",    
    "The number of subjects first at risk in each cell",
    "Unadjusted person years at risk",                       "Adjusted person years at risk",
    "Person-year weighted mean Ground distance in meters",   "Person-year weighted mean Age at exposure in years",
    "Person-year weighted mean Attained age in years",       "Person-year weighted mean year",
    
    "solid", "oralca", "lip", "tongue", "saliv", "mouth", "pharynx", 
    "digestca", "esoph", "stomach", "smallint", "colon", "rectum", 
    "liver", "gallbldr", "pancr", "othdig", "respca", "nasal", "larynx", 
    "lung", "Other cancers of the respiratory system", "thymus", "Cancer of bone, connective tissue and skin", 
    "bone", "connect", "Non-melanoma skin cancer", 
    "skbasal", "sksquam", "bowens disease", "breast", "femgenca", "uterus", 
    "utrNOS", "cervix", "corpus", "ovary", "othfem", "malgenca", 
    "prost", "testis", "othmale", "urinca", "bladder", "kidney", 
    "renal", "Other urinary system cancer", "Cancer of Central nervous system", "thyroid", "Other solid cancer", "other sites group", 
    "adeno", "squam", "Other epithelial cancers", "sarcoma", "Other non-epithelial cancers", "Histology not otherwise specified", 
    "colD","colg","coln", "marD","marg","marn", "braD", "breD", "livD", "livg", "livn", "lunD", 
    "ovaD", "panD", "Skeleton dose, mGy", "skiD", "stoD", "tesD", "thyD", "blaD", "uteD","truncation factor", "adjustment factor",
    "time since bombing","age Group","agex Group","t Group","Dose Group")
  
  nms=c(colName,added)
  df=data.frame(nms,desc)
  # df=data.frame(colName,desc)
  #   d=read.csv(file.path(AbombHome,"lssinc07.csv"))
  #   dput(names(d))
  d=read.csv(file.path(AbombHome,"lssinc07.csv"),col.names=colName)
  d=d%>%select(-ovaD,-tesD,-uteD)%>%mutate(D=marD,g=marg,n=marn,t=year-1945.6)
  # head(d,2)
  d$ageG=cut(d$age,c(0,20,40,60,70,80,110))
  d$agexG=cut(d$agex,c(0,20,40,110))
  d$tG=cut(d$t,c(0,10,20,40,110)) 
  d$DG<-cut(d$D,c(-1,.02,1,100),labels=c("Ctrl","<1Sv",">1Sv"))
  d$DG=factor(d$DG,levels=c(">1Sv","<1Sv","Ctrl"))#>1Sv at top
  # head(d,2)
  L=as.list(desc)
  names(L)=nms
  # labelled::var_label(d) <- L
  # View(d)
  d=d%>%select(city:un4gy,calg,ageG:DG,gdist:year,colD:t,py,upy,subjects,solid:histnos)
  # head(d,2)
  
  
  
  #   d=d%.%filter(doseg>1)%.%select(-ovaD,-tesD,-uteD)
  
  #   d=d%.%filter(marD>=0,py>0) #%.% 
  #   mutate(dose=cut(marD,c(0,.02,.4,10),include.lowest=TRUE,labels=c("low","med","high"))) 
  
  dbWriteTable(con,"solid",d,overwrite=TRUE)
  dbWriteTable(con,"solidDesc",df,overwrite=TRUE)
  solid=as_tibble(d)
  # solidDesc=as_tibble(df)
  save(heme,solid,file=f<-file.path(AbombHome,"abomb.RData"))
  
  cat("The following tables:\n")
  print(dbListTables(con)); 	
  dbDisconnect(con)
  cat("have been written to:",outDB,"\n")
  cat("and as dataframes to:",f,"\n")
}
