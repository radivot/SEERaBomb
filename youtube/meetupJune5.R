###### Meetup June 5th 2018
##  "Applications of the R Package SEERaBomb to Leukemia Risk Analyses"

####### Installing SEERaBomb  #############
# install.packages("SEERaBomb")
# install.packages("devtools")
# devtools::install_github("radivot/SEERaBomb",subdir="SEERaBomb") 

####### Getting the Data #############
#Get the SEER therapy ASCII data from  https://seer.cancer.gov/data/treatment.html
#Unzip this data into ~/data/SEER and run
library(SEERaBomb)  #loads installed package SEERaBomb into memory
(df=getFields())    #gets SEER fields into a data frame
(rdf=pickFields(df))#picks a subset of SEER fields and defines their types
# mkSEER(rdf)         #makes merged data file ~/data/SEER/mrgd/cancDef.Rdata
load("~/data/SEER/mrgd/cancDef.RData")#loads data.frame canc into memory
head(canc,3)                          #returns top 3 rows of canc

#Get A-bomb data------ 
#Go to http://www.rerf.or.jp/ and from "The incidence of leukemia ...:1950-2001” and 
#“Life Span Study Cancer Incidence Data, 1958-1998” get
#lsshempy.csv and lssinc07.csv and place them in ~/data/abomb. Then run
# mkAbomb()#this converts them to ~/data/abomb/abomb.RData
load("~/data/abomb/abomb.RData")#loads data frames heme and solid 
View(heme)                      #note descriptions under column names

#Get a Human Mortality Database username and password from http://www.mortality.org/ and run
# mkMrt("username","password")#sub in your account info
#to create ~/data/usMort/mrt.RData. 

####### Data Overview #############
canc
levels(canc$cancer)#returns a vector of all cancer types
sort(table(canc$cancer),decr=T) #reports numbers of each cancer type
load("~/data/SEER/mrgd/popsae.RData")
popsae
sum(popsae$py)/1e6 #in millions of PY
sum(popsae$py)/322e6 # ~6 us pop years
popsae%>%group_by(year)%>%summarize(PY=sum(py)/300e6)%>%ggplot(aes(x=year,y=PY))+geom_point()

####### CML Mortality #############
d=canc%>%filter(cancer=="CML")%>%print(n=13)
d%>%summarize(n=n(),na=sum(is.na(surv)),prct=100*na/n)#<2% missing
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)%>%print(n=13)
load("~/data/usMort/mrt.RData")#loads mrt
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1973,1990,2005,2015)))
(g=qplot(x=t,y=RR,data=D,col=Years,geom=c("line","point"),facets=.~sex,
         xlab="Years Since CML Diagnosis",ylab="Relative Risk of Mortality"))
(g=g+scale_x_continuous(breaks=seq(0,15,5)))
(g=g+theme(legend.position="top",legend.title=element_blank()))
(g=g+geom_abline(intercept=1,slope=0)+ylim(c(0,NA)))
(g=g+geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2))
ggsave("~/Results/june5/CMLmortRR.pdf",width=4.5,height=3)

library(ggsci);(g=g+scale_color_jco()+theme_classic(base_size=14))
(g=g+theme(legend.position="top",legend.title=element_blank(),
           strip.background=element_blank()))
quartz(width=4.5,height=3);g
ggsave("~/Results/june5/CMLmortJCO.pdf",width=4.5,height=3)


labs=c("1973-1990","1991-2005","2006-2015")
d=d%>%mutate(yrg=cut(yrdx,c(1972,1990,2005,2015),labels=labs))%>%print(n=13)
library(survival);library(survminer)
fit=survfit(Surv(surv,status)~yrg+sex,data=d) 
g=ggsurvplot_facet(fit,d,facet.by=c("sex"),ylab="Survival Probability",
                 xlab="Years Since CML Diagnosis",legend.title="",
                 xlim=c(0,12),short.panel.labs=T)+
  scale_x_continuous(breaks=seq(0,15,5))+
  scale_color_jco()+theme(strip.background=element_blank())
quartz(width=4.5,height=3);g
ggsave("~/Results/june5/CMLsurvTrendsJCO.pdf",width=4.5,height=3)


####### CML Incidence #############
(D=incidSEER(canc,popsae,"CML"))
(D=D%>%filter(age>=20,age<=80,year>=2000)%>%mutate(ageG=cut(age,seq(20,80,5))))
(D=D%>%group_by(sex,race,ageG)%>%
    summarize(age=mean(age),py=sum(py),n=sum(n))%>%mutate(incid=n/py))
library(bbmle)
summary(mm1<-mle2(n~dpois(lambda=exp(c+k*(age-50))*py),
                  #parameters=list(c~sex,k~sex),#k no diff
                  parameters=list(c~sex,k~race),#kW-O P=7e-8
                  method="Nelder-Mead",start=list(c=0,k=0.04),data=D)) 
D$EI=predict(mm1)/D$py#same as D=D%>%ungroup()%>%mutate(EI=predict(mm1)/py) 
qplot(age,incid,col=sex,shape=sex,data=D)+facet_grid(~race)+geom_line(aes(y=EI))+
  labs(x="Age (years)",y=quote(paste("CML Cases per ",10^5," Person Years")))+
  # scale_color_jco()+theme_classic(base_size = 13)+
  scale_y_log10(breaks=c(1,2,5))+scale_x_continuous(breaks=c(25,50,75))+
  theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
        # strip.background=element_blank(),
        legend.position=c(.45,.91),legend.key.height=unit(.7,'lines'))
ggsave("~/Results/june5/CMLincidJCO.pdf",width=4,height=3)

####### AML, ALL and CML relative risks (RR) after breast cancer ##########
library(tidyverse) # to load package forcats
canc$cancer=fct_collapse(canc$cancer,AML=c("AML","AMLti","APL"))
secs=c("AML","ALL","CML") 
(d=canc%>%filter(sex=="Female",cancer%in%c("breast",secs)))
pf=seerSet(d,popsae,Sex="Female")#pooled (races) females 
pf=mk2D(pf,secondS=secs)#adds secs background rates to pf
trts=c("rad.chemo","rad.noChemo","noRad.chemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="breast")
(dA=pf$DF%>%filter(ageG=="(0,60]")) 

myt=theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0),
          legend.direction="horizontal",legend.key.height=unit(.25,'lines'),
          legend.position=c(.5,.95),strip.background=element_blank())
ge=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=0.1)
gy=ylab("Relative Risk of Leukemia");cc=coord_cartesian(ylim=c(0,25))
gp=geom_point();gl=geom_line();gh=geom_hline(yintercept=1)
jco=scale_color_jco();tc=theme_classic(base_size=14)
gx=xlab("Years Since Breast Cancer Diagnosis")
g=ggplot(dA,aes(x=t,y=RR,col=cancer2))+gp+gl+gx+gy+gh+ge+cc
g+jco+tc+facet_grid(rad~chemo)+myt 
ggsave("~/Results/june5/breast2leuJCO.pdf",width=4,height=4) 

####### AML, ALL and CML incidence in A-bomb survivors ##########
load("~/data/abomb/abomb.RData")
(d=heme%>%select(ageG:DG,age,agex,t,D,py,AML=AMLtot,ALL,CML))
myt=theme(legend.position=c(.52,.85),legend.title=element_blank(),
          legend.direction="vertical",legend.margin=margin(0,0,0,0),
          legend.key.height=unit(.7,'lines'),strip.background=element_blank())
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.1)
gy=ylab(quote(paste("Cases per ",10^5," Person-Years")))
cc=coord_cartesian(ylim=c(.2,200));f=facet_wrap(~cancer);sy=scale_y_log10()
tc=theme_classic(base_size=12)
gx=xlab("Attained Age (Years)")
(dA=incidAbomb(d%>%group_by(ageG,DG)))
ggplot(dA,aes(x=age,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/june5/abombLeuAgeJCO.pdf",width=4,height=2.5)

gx=xlab("Age at Time of Bombing (Years)")
(dB=incidAbomb(d%>%group_by(agexG,DG)))
ggplot(dB,aes(x=agex,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/june5/abombLeuAgexJCO.pdf",width=4,height=2.5)

gx=xlab("Years Since Bombing")
(dC=incidAbomb(d%>%group_by(tG,DG)))
ggplot(dC,aes(x=t,y=I,shape=DG,col=DG))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco
ggsave("~/Results/june5/abombLeuTsxJCO.pdf",width=4,height=2.5)

gx=xlab("Dose (Sv)")
d$DG<-cut(d$D,c(-1,.02,.25,.5,.75,1.5,100))
(dD=incidAbomb(d%>%group_by(DG)))
ggplot(dD,aes(x=D,y=I))+gp+gl+gx+sy+gy+ge+f+cc+tc+myt+jco+
  scale_x_continuous(breaks=0:2)
ggsave("~/Results/june5/abombLeuDoseRespJCO.pdf",width=4,height=2.5)

# look at age-year incidence surfaces for liver cancer, NHL, and KS (Kaposi Sarcoma)
secs=c("liver","NHL","KS") 
(d=canc%>%filter(cancer%in%secs))
pf=seerSet(d,popsae,Sex="Female")#pooled (races) females 
pm=seerSet(d,popsae,Sex="Male")#pooled (races) males 
pf=mk2D(pf,secondS=secs)
pm=mk2D(pm,secondS=secs)
plot2D(pf)
plot2D(pm)

