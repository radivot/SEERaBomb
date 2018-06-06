#csdEx3.R
(d=canc%>%filter(sex=="Female",cancer%in%c("breast",secs)))
pf=seerSet(d,popsae,Sex="Female")#pooled (races) females 
pf=mk2D(pf,secondS=secs)#adds secs background rates to pf
trts=c("rad.chemo","rad.noChemo","noRad.chemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="breast")
(dA=pf$DF%>%filter(ageG=="(0,60]")) 
gy=ylab("Relative Risk of Leukemia")
myt=theme(legend.key.height=unit(.25,'lines'),legend.position=c(.5,.95))
cc=coord_cartesian(ylim=c(0,25))#clips high errorbars
gx=xlab("Years Since Breast Cancer Diagnosis")
dA%>%ggplot(aes(x=t,y=RR,col=cancer2))+facet_grid(rad~chemo)+ 
  gp+gl+gx+gy+gh+geRR+tc(14)+ltp+cc+jco+sbb+ltb+myt+lh
ggsave("~/Results/tutorial/breast2leu.pdf",width=4,height=4)#Fig.3A 

(d=canc%>%filter(cancer%in%c("thyroid",secs)))
pf=seerSet(d,popsae,Sex="Female");pm=seerSet(d,popsae,Sex="Male") 
pf=mk2D(pf,secondS=secs);pm=mk2D(pm,secondS=secs)
trts=c("rad.noChemo","noRad.noChemo")
pf=csd(pf,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="thyroid")
pm=csd(pm,brkst=c(0,1,2,3,5,10),brksa=c(0,60),trts=trts,firstS="thyroid")
DF=bind_rows(pf$DF,pm$DF)
D=DF%>%group_by(int,rad,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
Dtop=D%>%mutate(grp=str_c(rad,": All Ages"))
D=DF%>%filter(rad=="Rad")
D=D%>%group_by(int,ageG,cancer2)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D$ageG=c("Age 0-60","Age >60")[D$ageG]
Dbot=D%>%mutate(grp=str_c("Rad: ",ageG))
dB=bind_rows(Dtop,Dbot)
dB$grp=as_factor(dB$grp)#orders by occurrence, as wanted
gx=xlab("Years Since Thyroid Cancer Diagnosis")
dB%>%ggplot(aes(x=t,y=RR,col=cancer2))+facet_wrap(~grp)+ 
          gp+gl+gx+gy+RR1+geRR+tc(14)+myt+cc+jco+sbb+ltb+ltp+myt+lh
ggsave("~/Results/tutorial/thyroid2leu.pdf",width=4,height=4)#Fig.3B


