#incidEx2.R
(D=incidSEER(canc,popsae,"CML"))
(D=D%>%filter(age>=20,age<=80,year>=2000)%>%mutate(ageG=cut(age,seq(20,80,5))))
(D=D%>%group_by(sex,race,ageG)%>%
  summarize(age=mean(age),py=sum(py),n=sum(n))%>%mutate(incid=n/py))
summary(mm1<-mle2(n~dpois(lambda=exp(c+k*(age-50))*py),
                  #parameters=list(c~sex,k~sex),#k no diff
                  parameters=list(c~sex,k~race),#kW-O P=7e-8
                  method="Nelder-Mead",start=list(c=0,k=0.04),data=D)) 
D$EI=predict(mm1)/D$py#same as D=D%>%ungroup()%>%mutate(EI=predict(mm1)/py) 
D%>%ggplot(aes(x=age,y=incid,col=sex,shape=sex))+gxi+gyi+facet_grid(~race)+gp+
  geom_line(aes(y=EI))+scale_y_log10(breaks=c(1,2,5))+agts+jco+tc(13)+sbb+ltb+
  theme(legend.position=c(.45,.91),legend.key.height=unit(.7,'lines'))
ggsave("~/Results/tutorial/incidCML.pdf",width=4,height=3)

