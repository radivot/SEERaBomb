###msdEx1.R 
d=canc%>%filter(cancer=="CML")%>%print(n=13)
d%>%summarize(n=n(),na=sum(is.na(surv)),prct=100*na/n)#<2% missing
d=d%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
d=d%>%select(yrdx,agedx,sex,surv,status)%>%print(n=13)
(D=msd(d,mrt,brkst=c(0,0.5,1,2,3,4,5,6,8),brksy=c(1975,1990,2005,2016)))
gx=xlab("Years Since CML Diagnosis")
gy=ylab("Relative Risk of Mortality")
myt=theme(legend.text=element_text(size=12),strip.text=element_text(size=12))
D%>%ggplot(aes(x=t,y=RR,col=Years))+facet_grid(.~sex)+gp+gl+gx+gy+gh+
       svts+jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR+myt
ggsave("~/Results/tutorial/msdEx1A.pdf",width=4.5,height=3)

labs=c("1975-1990","1991-2005","2006-2016")
d=d%>%mutate(yrg=cut(yrdx,c(1974,1990,2005,2016),labels=labs))%>%print(n=13)
fit=survfit(Surv(surv,status)~yrg+sex,data=d) 
gy=ylab("Survival Probability")
ggsurvplot_facet(fit,d,facet.by="sex",legend.title="",xlim=c(0,12),
          short.panel.labs=T)+svts+jco+sbb+myt+gx+gy
ggsave("~/Results/tutorial/survEx1B.pdf",width=4.5,height=3)



