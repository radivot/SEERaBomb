load("data/allfields2019.RData")
d$CODS=as.character(d$CODS)
d=d%>%mutate(CODS=ifelse(!CODS%in%c("alive","melanoma","eye"),"other",CODS))
d$CODS[d$CODS=="eye"]="melanoma"
#Figure 1
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%mutate(survG=cut(surv,c(0,5,10,15,20,25,30,45),
                       dig.lab=4,include.lowest=T,right=F))
m=d
table(m$survG,m$CODS) # make this in dplyr
library(tidyr)
(fg1=m%>%group_by(survG,CODS)%>%summarize(n=n()))
(fg1=fg1%>%spread(CODS,n))
# (fg1=fg1%>%mutate(n=other+melanoma))
# (fg1=fg1%>%mutate(frc=other/n))
# fg1%>%group_by()%>%summarize(M=sum(melanoma),O=sum(other),P=O/(M+O))
library(WriteXLS)
WriteXLS(list(Fig1=fg1),ExcelFileName="outs/fig1.xlsx")
# fraction of deaths by UVM in all vs SEER9
(tb=table(d$CODS))
sum(tb[2:3])
tb[2]/sum(tb[2:3])
d9=d%>%filter(db=="73")
(tb=table(d9$CODS))
sum(tb[2:3])
tb[2]/sum(tb[2:3])
table(d$db)

d9=d9%>%filter(yrdx<1987)
(tb=table(d9$CODS))
sum(tb[2:3])
tb[2]/sum(tb[2:3])
tb[2]/sum(tb)

#Table 1 using’s SEERaBombs mkDemographics()
load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
load("~/data/SEER/mrgd/popsae.RData")#load SEER population data
head(canc,2)
canc=canc%>%filter(primsite%in%c("C693","C694","C692"),histo3%in%8720:8790)%>%print(n=2)
canc$cancer=as.character(canc$cancer)
# C69.3 (choroid), C69.4 (ciliary body and iris), and C69.2 (retina). 
canc$cancer[canc$primsite=="C693"]="Choroid" 
canc$cancer[canc$primsite=="C694"]="Ciliary"
canc$cancer[canc$primsite=="C692"]="Retinal"
canc%>%filter(is.na(surv))%>%print(n=4) # one choroid survival is missing so tables will be shy 1 for it
d=canc%>%filter(!is.na(surv))%>%print(n=4)
d$age=cut(d$agedx,c(0,40,60,80,126),include.lowest = T)
d$year=cut(d$yrdx,c(1975,1995,2016),include.lowest = T,dig.lab=4)
mkDemographics(d,outDir="outs")
d$cancer="uveal"
mkDemographics(d,outDir="outs")


# There are many field choices in SEER data and not all are applicable to a
# specific cancer. To obtain a bird’s eye view of uveal melanoma field use, this
# following script generates a file uvealAllFields.xlsx that has 153 sheets (one
# per field), each with counts of levels. If a distribution is not piled up at
# mostly one value, the field may be of interest. Sheet tab labels are defined
# in fieldDefsAPR2019.xlsx.  The following script generates both of these excel
# files. The commented code is slow, but it only needs to run once per year per
# machine, so uncomment it, run it, and then comment it again to avoid wasting
# time accidentally running it twice.


(df=getFields("~/data/SEER")) # in windows ~ maps to /users/radivot/documents
WriteXLS(df,ExcelFileName="outs/fieldDefs2019.xlsx") # field definitions

# system.time(load("~/data/SEER/mrgd/cancALL.RData")) # takes ~65 secs
# d=canc%>%filter(primsite%in%c("C693","C694","C692"))%>%print(n=2)
# d=d%>%filter(histo3%in%8720:8790)%>%print(n=2)
# d=d%>%filter(!is.na(surv))%>%print(n=4)
# save(d,file="uveal/data/allfields2019.RData") #just the 10678 cases studied
load("data/allfields2019.RData")   
nms=names(d)
L=lapply(nms,function(x) {dd=as.data.frame(table(d[,x],useNA="always"))
if (dim(dd)[2]==2) names(dd)=c("Variable","Frequency")
dd} )
names(L)=nms
WriteXLS(L,ExcelFileName="outs/uvealAllFieldsNA.xlsx",AdjWidth=T)
