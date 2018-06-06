(sc=canc%>%filter(histo3==9920|histo3==9987,yrdx>2000))#subset canc
(d=sc%>%count(sex,yrdx,histo3))#n in d holds sex-year-ICDO3 group row counts 
d%>%ggplot(aes(x=yrdx,y=n))+facet_grid(sex~histo3)+ylab("Cases")+gp+
 xlab("Year of Diagnosis")+scale_x_continuous(breaks=seq(2005,2015,5))+tc(11)+sbb
ggsave("~/Results/tutorial/tMDSrepaired.pdf",width=3,height=2.5) 
# quartz(width=3,height=2.5);g;ggsave("~/Results/tutorial/tMDSrepaired.pdf") 
# options(device = "RStudioGD")