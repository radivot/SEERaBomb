### fig1abcTab1.R    Figure 1abc and Table 1  and calculations in the text 
# source("~/case/active/seer/CML/naga/common.R") #run it first directly
load("~/data/abomb/abomb.RData")#loads data frames heme and solid 
(d=heme%>%select(city,sex,ageG:DG,age,year,t,D,py,CML))
d%>%filter(CML>0)%>%print(n=100)
d$ageG=cut(d$age,c(0,20,40,60,80,126),include.lowest =T)
d$DG="Unknown"  #negative doses are unknown
d$DG[d$D==0]="Not in City"
d$DG[d$D>0&d$D<0.2]="<0.2 Sv"
d$DG[d$D>=0.2&d$D<=1]="0.2-1 Sv"
d$DG[d$D>1]=">1 Sv" #next line puts >1Sv at top of legends
d$DG=factor(d$DG,levels=c(">1 Sv","0.2-1 Sv","<0.2 Sv","Not in City","Unknown"))
(dA=incidAbomb(d%>%group_by(ageG,DG)))
# get SEER groups
load("~/data/SEER/mrgd/popsae.RData") 
(D=incidSEER(canc,popsae,"CML")) #%>%filter(race=="Other"))
D=D%>%mutate(ageG=cut(age,c(0,20,40,60,80,126),include.lowest=T))%>%
  group_by(cancer,race,ageG)%>%summarize(age=mean(age),py=sum(py),O=sum(n))%>%
  mutate(I=O/py,LL=qpois(0.025,O)/py,UL=qpois(0.975,O)/py)
D$DG=paste0("SEER ",D$race)
D=bind_rows(dA,D) 
D=D%>%filter(!DG%in%c(">1 Sv","0.2-1 Sv","Unknown")) # next line puts SEER at bottom
D$DG=factor(D$DG,levels=c("<0.2 Sv","Not in City","SEER White","SEER Black","SEER Other"))
D$age=D$age+(as.integer(D$DG)-3)/4 #shift on both sides to see CI
myt=theme(legend.position=c(.52,.85),legend.key.height=unit(.7,'lines'))
cc=coord_cartesian(ylim=c(.2,15));f=facet_wrap(~cancer)
gx=xlab("Attained Age (Years)");gp3=geom_point(size=3)
ggplot(D,aes(x=age,y=I,shape=DG))+gp3+gl+gx+sy+gyi+ge+cc+tc(14)+myt+jco+sbb+ltb
ggsave("~/Results/CML/abombAgeFig1A.pdf",width=4,height=3.2)
# END creating Figure 1A
# Begin creating Table 1
d=d%>%mutate(city=ifelse(city==1,"Hiroshima","Nagasaki"))
d=d%>%mutate(sex=as_factor(ifelse(sex==1,"Male","Female")))
d1=d%>%group_by(city,sex,DG,ageG)%>%  #set aside A-bomb as d1
  summarize(t=weighted.mean(t,w=CML),CML=sum(CML),age=mean(age),py=sum(py))
(D=incidSEER(canc,popsae,"CML")) #recreate to make sex specific rates for E estimates
(D=D%>%filter(year>=2000)%>%mutate(ageG=cut(age,c(0,20,40,60,80,126),include.lowest =T)))
(D=D%>%group_by(sex,race,ageG)%>%
    summarize(age=mean(age),py=sum(py),n=sum(n))%>%mutate(incid=n/py))
(dS=ungroup(D)%>%filter(race=="Other")%>%select(sex,ageG,incidence=incid))
(dB=left_join(d1,dS))
dB=dB%>%mutate(E=incidence*py/1e5)
dB=dB%>%select(city,sex,DG,ageG,O=CML,E,t)
dB=ungroup(dB)%>%mutate(E=round(E,3),t=round(t,1),t=ifelse(is.nan(t),NA,t))
dB=dB%>%filter(as.integer(DG)<=3) #leave out NIC and UNK DG in tables
library(openxlsx)
hs1=createStyle(fgFill="#DDDDDD",halign="CENTER",textDecoration="bold")
hs2=createStyle(fgFill="#FFFFFF",halign="CENTER",textDecoration="bold")
unlink(f<-"~/Results/CML/Table1.xlsx")
wb <- createWorkbook() 
addWorksheet(wb,"NH")
(tbm=dB%>%filter(city=="Nagasaki",sex=="Male")%>%select(-city,-sex))
(tbf=dB%>%filter(city=="Nagasaki",sex=="Female")%>%select(-city,-sex))
snm=tbm%>%group_by(DG)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
snf=tbf%>%group_by(DG)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
snf$t[is.nan(snf$t)]=NA
writeData(wb,"NH",data.frame("1950-2001"), startRow=1,colNames=F)
mergeCells(wb,"NH", cols = 4:9, rows = 1)
writeData(wb,"NH",data.frame("                                  Nagasaki"), startRow=1,startCol=4,colNames=F,headerStyle = hs2)
# writeData(wb,"NH",data.frame("Totals"), startRow=19,colNames=F)
mergeCells(wb,"NH", cols = 4:6, rows = 2)
mergeCells(wb,"NH", cols = 7:9, rows = 2)
writeData(wb,"NH",data.frame("               Males"), startRow=2,startCol=4,colNames=F)
writeData(wb,"NH",data.frame("             Females"), startRow=2,startCol=7,colNames=F)
names(tbm)=c("Dose","Age","Obs","Exp","Tsx")
names(tbf)=c("Dose","Age","Obs","Exp","Tsx")
# names(snm)=c("Totals","Obs","Exp")
# names(snf)=c("Dose","Obs","Exp")
writeData(wb,"NH",tbm[1:2], startRow=3,startCol=1,headerStyle = hs1)
writeData(wb,"NH",tbm[3:5], startRow=3,startCol=4,headerStyle = hs1)
writeData(wb,"NH",tbf[3:5], startRow=3,startCol=7,headerStyle = hs1)
writeData(wb,"NH",snm[1],   startRow=19,startCol=1,colNames=F)
writeData(wb,"NH",data.frame(rep("all",3)), startRow=19,startCol=2,colNames=F)
writeData(wb,"NH",snm[2:4],  startRow=19,startCol=4,colNames=F)
writeData(wb,"NH",snf[2:4],  startRow=19,startCol=7,colNames=F)
setColWidths(wb, "NH", cols=4:16, widths = 4)
setColWidths(wb, "NH", cols=c(5,8,12,15), widths = 6)
setColWidths(wb, "NH", cols=c(3,10), widths = 0.25)
(tbm=dB%>%filter(city=="Hiroshima",sex=="Male")%>%select(-city,-sex))
(tbf=dB%>%filter(city=="Hiroshima",sex=="Female")%>%select(-city,-sex))
shm=tbm%>%group_by(DG)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
shf=tbf%>%group_by(DG)%>%summarize(O=sum(O),E=sum(E),t=mean(t,na.rm=T))
mergeCells(wb,"NH", cols = 11:16, rows = 1)
mergeCells(wb,"NH", cols = 11:13, rows = 2)
mergeCells(wb,"NH", cols = 14:16, rows = 2)
writeData(wb,"NH",data.frame("                                   Hiroshima"), startRow=1,startCol=11,colNames=F)
writeData(wb,"NH",data.frame("               Males"), startRow=2,startCol=11,colNames=F)
writeData(wb,"NH",data.frame("             Females"), startRow=2,startCol=14,colNames=F)
names(tbm)=c("Dose","Age","Obs","Exp","Tsx")
names(tbf)=c("Dose","Age","Obs","Exp","Tsx")
writeData(wb,"NH",tbm[3:5],      startRow=3,startCol=11,headerStyle = hs1)
writeData(wb,"NH",tbf[3:5], startRow=3,startCol=14,headerStyle = hs1)
writeData(wb,"NH",shm[2:4],  startRow=19,startCol=11,colNames=F)
writeData(wb,"NH",shf[2:4],  startRow=19,startCol=14,colNames=F)
saveWorkbook(wb,file=f,overwrite = TRUE)
###### END creating Table 1
###### below are calculations given in the text
library(rateratio.test)
(hn2=dB%>%filter(ageG!="[0,20]",DG==">1 Sv"|DG=="0.2-1 Sv")%>%
  group_by(city)%>%summarize(O=sum(O),E=sum(E)))
rateratio.test(c(0,29),c(1.54,5)) #adults P=0.0008
(hn1=dB%>%filter(ageG=="[0,20]",DG==">1 Sv"|DG=="0.2-1 Sv")%>%
  group_by(city)%>%summarize(O=sum(O),E=sum(E)))
rateratio.test(c(2,3),c(0.01,0.028)) #children P=0.8
(x=dpois(0:10,0.01)) 
1-sum(x[1:2]) #Nagasaki P value for difference from RR=1 
(x=dpois(0:10,0.03)) #same for Hiroshima
1-sum(x[1:3]) 
# city difference P values below
(hn=dB%>%filter(DG=="<0.2 Sv")%>%
    group_by(city)%>%summarize(O=sum(O),E=sum(E)))
rateratio.test(hn$O,hn$E) # low doses, all ages combined
O=24; E=25.6
round(c(O/E,qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E)),2)
O=9; E=11.3
round(c(O/E,qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E)),2)

(t1=dB%>%filter(ageG=="[0,20]",DG=="<0.2 Sv")%>%
    group_by(city)%>%summarize(O=sum(O),E=sum(E)))
rateratio.test(t1$O,t1$E) #low doses, young
(t1=dB%>%filter(ageG!="[0,20]",DG=="<0.2 Sv")%>%
    group_by(city)%>%summarize(O=sum(O),E=sum(E)))
rateratio.test(t1$O,t1$E)#low doses, NOT young 
## Create RR CI
(HN=bind_rows(hn2,hn1,hn))
(HN=HN%>%mutate(RR=O/E,low=qchisq(0.025, 2*O)/(2*E),hi=qchisq(0.975, 2*O+2)/(2*E)))
O=0; E=1.5
round(c(O/E,qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E)),2)
O=29; E=5; O/E
c(qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E))
O=2;E=0.01; O/E
c(qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E))
O=3; E=0.03; O/E
c(qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E))
### make forest plot
graphics.off()
quartz(width=7,height=2.2)
myticks=c(-1,0,1,2,3)
attr(myticks,"labels")=as.character(c(0.1,1,10,100,1000))
dput(myticks)
(myticks)
library(forestplot)
text <- bind_cols(t1=c("","City",NA,HN$city),
             t2=c("","Age (y)",NA,c(rep(">20",2),rep("<20",2),rep("All",2)) ),
             t3=c("","Dose (mSv)",NA,c(rep(">200",4),rep("<200",2)) ))
text <- bind_cols(t1=c("City",NA,HN$city),
                  t2=c("Age (y)",NA,c(rep(">20",2),rep("<20",2),rep("All",2)) ),
                  t3=c("Dose (mSv)",NA,c(rep(">200",4),rep("<200",2))),
                  t4=c("Obs/Exp",NA,paste0(HN$O,"/",round(HN$E,2))))
text
forestplot(text, 
           mean=c(rep(NA,2),log10(HN$RR)),
           lower=c(rep(NA,2),log10(HN$low)),
           upper=c(rep(NA,2),log10(HN$hi)),
           # title="Nagasaki Mystery",
           xlab="Relative Risk of CML",
           col=fpColors(box="black", lines="black", zero = "gray50"),
           zero=-1,
           xticks=myticks,
           boxsize=0.3,
           colgap=unit(3,"mm"),
           lineheight = unit(7,"mm"),
           lwd.ci=1.3, ci.vertices=TRUE, ci.vertices.height = 0.2,
           clip=c(-1,3),
           txt_gp=fpTxtGp(label=gpar(cex=1),
                          ticks=gpar(cex=1),
                          xlab=gpar(cex = 1),
                          title=gpar(cex = 1))
           )  #end forest plot
## END RR CI
## Get P value for longer delays in HF 
d3m=d%>%filter(DG==">1 Sv",city=="Hiroshima",CML==1,sex=="Male")
d3f=d%>%filter(DG==">1 Sv",city=="Hiroshima",CML==1,sex=="Female")
wilcox.test(d3f$t,d3m$t) #P=0.02
quantile(d3f$t)
quantile(d3m$t) 
(L=list(F=sort(round(d3f$t,1)),M=sort(round(d3m$t,1))))
lapply(L,paste0,collapse=", ")
Dt=data.frame(t=c(d3f$t,d3m$t),Sex=c(rep("Female",6),rep("Male",10)))
graphics.off()
Dt%>%ggplot(aes(x=Sex,y=t))+geom_boxplot(alpha=0)+geom_jitter(width=.15)+
  ylab("CML Latency (Years)")+tc(14)
ggsave("~/Results/CML/latencyFig2A.pdf",width=4,height=2.8)
heme%>%filter(DG==">1Sv",city==1,CML==1)%>%select(sex,D,age,t)
dB%>%filter(DG==">1 Sv")%>%group_by(city)%>%summarize(O=sum(O),E=sum(E))
dB%>%filter(DG==">1 Sv")%>%group_by(city,sex)%>%summarize(O=sum(O),E=sum(E))
O=16;E=0.96;O/E
c(qchisq(0.025, 2*O)/(2*E),qchisq(0.975, 2*O+2)/(2*E))
# Get P values for young being all Males
rateratio.test(c(2,0),c(0.006,0.004), alt="greater")  # N young M vs. F
rateratio.test(c(3,0),c(0.017,0.011), alt="greater")#H young M vs. F
rateratio.test(c(5,0),c(0.023,0.015), alt="greater")#HN young M vs. F
# show no other diffs in Nagasaki
rateratio.test(c(5,4),c(5.5,5.9), alt="greater")#N low dose total
rateratio.test(c(2,0),c(0.8,0.76), alt="greater")#N >200 mSv, M vs. F, all ages
rateratio.test(c(7,3),c(0.96,1.36), alt="greater")  # P=0.066  old M vs. F
# Neutron dose differences between cities
head(heme)
heme%>%filter(D>0.2)%>%group_by(city)%>%summarize(ratio=1-mean(g/D,na.rm=T))
heme%>%group_by(city)%>%summarize(ratio=weighted.mean(g/D,na.rm=T,weight=py))
# Get ATL city differences
(d=heme%>%select(city,sex,ageG:DG,age,year,t,D,py,ATL)) 
d=d%>%mutate(city=ifelse(city==1,"Hiroshima","Nagasaki"))
d=d%>%mutate(sex=as_factor(ifelse(sex==1,"Male","Female")))
(da=d%>%group_by(city)%>%summarize(t=weighted.mean(t,w=ATL),
                                   ATL=sum(ATL),age=mean(age),
                                   py=sum(py)/1e5,incid=ATL/py))
rateratio.test(da$ATL,da$py) 
(da=d%>%group_by(city,sex)%>%summarize(t=weighted.mean(t,w=ATL),
                                       ATL=sum(ATL),age=mean(age),
                                       py=sum(py)/1e5,incid=ATL/py))
da%>%mutate(rrL=qchisq(.025,2*ATL)/(2*py),rrU=qchisq(.975,2*ATL+2)/(2*py))
## Standarized incidence: compare SEER race other vs. white
(D=incidSEER(canc,popsae,"CML")) #recreate to get sex specific rates 
(D=D%>%filter(year>=2000)%>%group_by(race,age)%>%
    summarize(py=sum(py),n=sum(n))%>%mutate(incid=n/py))
D%>%summarize(py=sum(py*stdUS$prop),n=sum(n))%>%mutate(incid=n/py)
82.7/107 #77% not quite 50% as below but in right direction
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3907701/pdf/bjh0164-0536.pdf
(d=heme%>%select(city,sex,ageG:DG,age,year,t,D,py,CML)) 
d=d%>%mutate(city=ifelse(city==1,"Hiroshima","Nagasaki"))
(db=d%>%filter(city=="Hiroshima",CML==1)%>%mutate(Sex=c("M","F")[sex]))
db%>%ggplot(aes(x=Sex,y=t))+geom_boxplot(alpha=0)+geom_jitter(width=.15)+facet_grid(~DG)+
  ylab("Hiroshima CML Latency (Years)")+tc(14)+sbb
ggsave("~/Results/CML/hiroCMLlatencyNIC.pdf",width=5,height=3)
(db1=db%>%group_by(DG)%>%nest())
d1=db1$data[[1]]
db1=db1%>%mutate(wc=map(data,function(x) wilcox.test(t~sex,data=x)))
db1%>%mutate(P=map_dbl(wc,function(x) x$p.value))%>%select(DG,P)
