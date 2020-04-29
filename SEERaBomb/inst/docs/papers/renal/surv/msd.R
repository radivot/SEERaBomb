source("renal/common/load.R")
(Ds=msd(d5,mrt,brkst<-c(0,0.5,1,2,3,4,5,6,8,10,12,15,20,25,30,35)))
D=Ds%>%group_by(int)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D1=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
source("renal/common/acros.R")
gy=ylab("Relative Risk of Mortality")
D1%>%ggplot(aes(x=t,y=RR))+gp+gl+gx+gy+gh+tc(14)+geRR
ggsave("renal/outs/msdPool.pdf",width=4.5,height=3)
Ds%>%ggplot(aes(x=t,y=RR,col=sex))+gp+gl+gx+gy+gh+tc(14)+geRR+ltb+ltp
ggsave("renal/outs/msdSex.pdf",width=4.5,height=3)

# check calendar year changes
(Dsy=msd(d5,mrt,brkst=c(0,0.5,1,2,3,4,5,6),brksy=c(1975,1990,2005,2016)))
D=Dsy%>%group_by(Years,int)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
labs=c("1975-1990","1991-2005","2006-2016")
D%>%ggplot(aes(x=t,y=RR,col=Years))+gp+gl+gx+gy+gh+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR #+myt
ggsave("renal/outs/msdYear.pdf",width=4.5,height=3)

# show RR high in young (due to low denom rates)
hist(d5$agedx)
d6=d5%>%mutate(ageG=ifelse(agedx>60,"Old (>60)","Young (<=60)"))
d6=d6%>%group_by(ageG)%>%nest()
(d6=d6%>%mutate(m=map(data,function(x) msd(x,mrt,brkst))))
(D=d6%>%select(-data)%>%unnest(m))
D=D%>%group_by(ageG,int)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D%>%ggplot(aes(x=t,y=RR,col=ageG))+gp+gl+gx+gy+gh+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR
ggsave("renal/outs/msdAgeG.pdf",width=4.5,height=3)

## look for diff in treatment
(d6=d%>%select(yrdx,agedx,sex,surv,status,trt))
d6=d6%>%group_by(trt)%>%nest()
d6=d6[1:4,]
d6$trt=c("No Rad, No Chemo","Rad, No Chemo","No Rad, Chemo","Rad, Chemo")
(d6=d6%>%mutate(m=map(data,function(x) msd(x,mrt,brkst))))
(D=d6%>%select(-data)%>%unnest(m))
D=D%>%group_by(trt,int)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
D%>%ggplot(aes(x=t,y=RR,col=trt))+gp+gl+gx+gy+gh+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR
ggsave("renal/outs/msdTrt.pdf",width=7,height=3)

## Disease specific mortality 
table(d$CODS)
table(d$status)
(d6=d%>%select(yrdx,agedx,sex,surv,status,CODS))
(d6=d6%>%mutate(status=ifelse((status==1)&(!CODS%in%c("renal")),0,status))%>%select(-CODS))
(Ds=msd(d6,mrt,brkst)) # why are NA getting in there
D=Ds%>%group_by(int)%>%summarize(O=sum(O),E=sum(E),t=mean(t))
D=D%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))
gy=ylab("Relative Risk of Renal Death")
D%>%ggplot(aes(x=t,y=RR))+gp+gl+gx+gy+gh+geom_hline(yintercept=0)+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR
ggsave("renal/outs/msdRenalSpecific.pdf",width=4.5,height=3.5)

D1$grp="All Cause Mortality"
D$grp="Death by Renal Cancer"
gy=ylab("Relative Risk of Death")
bind_rows(D1,D)%>%ggplot(aes(x=t,y=RR,col=grp))+gp+gl+gx+gy+gh+geom_hline(yintercept=0)+
  jco+tc(14)+ltb+ltp+sbb+ylim(c(0,NA))+geRR
ggsave("renal/outs/msdBoth.pdf",width=4.5,height=3.5)
