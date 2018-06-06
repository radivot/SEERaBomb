#ageTherapyEx4.R
d=incidSEER(canc,popsae,secs)
d=d%>%filter(age<=85,year>=2000)
d=d%>%mutate(ageG=cut(age,seq(0,85,5)))
d=d%>%group_by(cancer,ageG)%>%
  summarize(age=mean(age),py=sum(py),n=sum(n))%>%
  mutate(incid=n/py,grp="Background")
d=d%>%select(cancer,grp,everything(),-ageG)#reorder columns
NHM=c("breast","thyroid","brain","renal")#NHM=non-heme malignancy
brksa=c(0,40,50,60,70,80)#broad 1st interval avoids 0 CML groups
system.time(ra<-riskVsAge(canc,firstS=NHM,secondS=secs,brksa=brksa))#~15s
raCML=riskVsAge(canc,firstS=c("AML","ALL"),secondS="CML",brksa=brksa)
D=bind_rows(ra,raCML)
D=D%>%filter(rad!="Unk",chemo!="Unk")
D=D%>%group_by(cancer2,rad,chemo,age)%>%
  summarize(py=sum(py),n=sum(o),incid=n/py)
D=D%>%rename(cancer=cancer2)%>%unite(grp,rad,chemo,sep=", ")
dd=bind_rows(D,d)%>%filter(age>=20)
ord=c("Rad, Chemo","No Rad, Chemo","Rad, No Chemo",
      "No Rad, No Chemo","Background")
dd$grp=factor(dd$grp,levels=ord)
dd$cancer=factor(dd$cancer,levels=secs)
myt=theme(legend.position=c(.67,.78),legend.key.height=unit(.65,'lines'))
dd%>%ggplot(aes(x=age,y=incid,col=grp))+gl+facet_grid(~cancer)+gxi+agts+gyi+jco+
  sy+tc(11)+ltb+sbb+myt
ggsave("~/Results/tutorial/seerLeu.pdf",width=3.5,height=2.5)#Fig.4

# dd=dd%>%mutate(LL=qpois(0.025,n)/py,UL=qpois(0.975,n)/py) #look at some CI
# dd%>%filter(cancer=="CML",grp%in%c("Rad, Chemo","Rad, No Chemo"))%>%arrange(age)
# dd%>%filter(cancer=="AML",grp%in%c("Rad, Chemo","No Rad, Chemo"))%>%arrange(age)
# dd%>%filter(cancer=="ALL",grp%in%c("Rad, Chemo","No Rad, Chemo"))%>%arrange(age)
