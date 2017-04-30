d=read.csv("~/data/SEER/crt.csv")
names(d)=c("casenum","seqnum","RT","CT")
crt=d%>%select(casenum,seqnum,CT,RT)
save(crt,file="~/data/SEER/crt.RData")
head(crt)
load("~/data/SEER/crt.RData")
load("~/data/SEER/mrgd/cancDef.RData")
levels(crt$seqnum)
head(crt$seqnum)
x=strsplit(as.character(crt$seqnum), "[^0-9]+")
crt$seq=sapply(x,function(y) y[[1]])
crt$seq[crt$seqnum=="One primary only"]=0
crt$seq[crt$seqnum=="Only one state registry-defined neoplasm"]=60
crt$seq[crt$seqnum=="Unknown seq num - federally required in situ or malig tumors"]=99
crt$seq[crt$seqnum=="Unknown seq num - state registry-defined neoplasms"]=88
(S=paste0(1:27,"th of ",1:27," or more state registry-defined neoplasms"))
for (i in 1:27)  crt$seq[crt$seqnum==S[i]] = 60+i
crt$seq=as.integer(crt$seq)
crt$seq[1:800]
crt$ct=as.integer(crt$CT)-1
levels(crt$RT)
crt$radiatn=9  # 9 is gone, so shouldn't be any left after below
crt$radiatn[crt$RT=="None/Unknown"]=0
crt$radiatn[crt$RT=="Beam radiation"]=1
crt$radiatn[crt$RT=="Radioactive implants"]=2
crt$radiatn[crt$RT=="Radioisotopes"]=3
crt$radiatn[crt$RT=="Combination of beam with implants or isotopes"]=4
crt$radiatn[crt$RT=="Radiation, NOS  method or source not specified"]=5
crt$radiatn[crt$RT=="Other radiation (1973-1987 cases only)"]=6
crt$radiatn[crt$RT=="Refused"]=7
crt$radiatn[crt$RT=="Recommended, unknown if administered"]=8
head(crt,20)
cr=crt%>%select(casenum,seqnum=seq,ct,radiatn)
d=left_join(canc,cr)
canc=d
canc$trt="noRad"
canc$trt[canc$radiatn==8]="unk"
canc$trt[canc$radiatn%in%c(1:6)]="rad"
save(canc,file="~/data/SEER/cancCRT.RData")

