###ageTherapyEx4.R
d=incidSEER(canc,popsae,secs)
d=d%>%filter(age<=85,year>=2000)
d=d%>%mutate(ageG=cut(age,seq(0,85,5)))
d=d%>%group_by(cancer,ageG)%>%
  summarize(age=mean(age),py=sum(py),n=sum(n))%>%
  mutate(incid=n/py,grp="Background")
d=d%>%select(cancer,grp,everything(),-ageG)#reorder columns
#the next 3 lines defines a set of non-heme-malignacies
HM=c(secs,"MDS","CMML","MPN","CLL","HCL","OL","NHL","MM","HL","LGL")
sc=canc%>%filter(agedx<50,yrdx<2000)#based on frequency at younger ages
(NHM=setdiff(names(sort(table(sc$cancer),decr=T)[1:8]),HM))#non-hemes
brksa=c(0,40,50,60,70,80)#broad 1st interval avoids 0 CML groups
system.time(D<-riskVsAge(canc,firstS=NHM,secondS=secs,brksa=brksa))#~36s
D=D%>%filter(rad!="Unk",chemo!="Unk")
D=D%>%group_by(cancer2,rad,chemo,age)%>%
  summarize(py=sum(py),n=sum(o),incid=n/py)
D=D%>%rename(cancer=cancer2)%>%unite(grp,rad,chemo,sep=", ")
dd=bind_rows(D,d)%>%filter(age>=20)
ord=c("Rad, Chemo","No Rad, Chemo","Rad, No Chemo",
      "No Rad, No Chemo","Background")
dd$grp=factor(dd$grp,levels=ord)
dd$cancer=factor(dd$cancer,levels=secs)
myt=theme(legend.position=c(.7,.83),legend.key.height=unit(.65,'lines'))
dd=dd%>%mutate(LL=qpois(0.025,n)/py,UL=qpois(0.975,n)/py)#make CI
dd=dd%>%mutate(ages=age+(as.numeric(grp)-3))#shift ages to see CI
dd%>%ggplot(aes(x=ages,y=incid,col=grp))+gl+facet_grid(~cancer)+gxi+
  agts+gyi+jco+sy+tc(11)+ltb+sbb+myt+ge
ggsave("~/Results/tutorial/ageTherapyEx4.pdf",width=3.5,height=2.5)
