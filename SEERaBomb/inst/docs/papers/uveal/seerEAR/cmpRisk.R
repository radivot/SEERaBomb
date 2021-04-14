library(cmprsk) #run after surv.r
(d=d%>%select(yrdx,agedx,sex,surv,status,CODS))
D2=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),2,status))
fitc=with(D2,cuminc(surv,status))
labs=c("Death by Melanoma","Death by Other Causes")
# plot(fitc,curvlab=labs,xlab="Years Since Diagnosis",ylab="Cumulative Probability of Death")
# pdf("outs/cumIncEstimates.pdf",width=4,height=4)
# plot(fitc,curvlab=labs,xlab="Years Since Diagnosis",ylab="Cumulative Probability of Death",ylim=c(0,1))
names(fitc)=c(" 1"," 2")  # gets rid of 1 at top
(g=ggcompetingrisks(fitc)+sbb+gx+ylab("Cumulative Probability of Death")+
    scale_color_manual(labels=labs,values=c("#F8766D","#00BFC4"))+ylim(c(0,1))+
    theme(legend.position = c(0.3, 0.8),legend.title=element_blank()) +labs(title="",caption=""))
ggsave("outs/cumIncY0to1.pdf",width=4.5,height=3.5) #Fig. 3
library("MALDIquant")
dmfs=as_tibble(fitc[[1]])
dos=as_tibble(fitc[[2]])
x0=seq(5,35,5)
(kos=match.closest(x0,dos$time))
(dos=dos[kos,]%>%mutate(CIos=paste0(sprintf('%.3f',est)," (",
                                           sprintf('%.3f',est-1.96*sqrt(var)),", ",
                                           sprintf('%.3f',est+1.96*sqrt(var)),")")))
(kmfs=match.closest(x0,dmfs$time))
(dmfs=dmfs[kmfs,]%>%mutate(CImfs=paste0(sprintf('%.3f',est)," (",
                                               sprintf('%.3f',est-1.96*sqrt(var)),", ",
                                               sprintf('%.3f',est+1.96*sqrt(var)),")")))

L=list(CumInc=tibble(Time=x0, "All Death Cumulative Incidence"=dos$CIos, #table 3
                     "Melanoma Death Cumulative Incidence"=dmfs$CImfs))
WriteXLS(L,ExcelFileName="outs/cumIncEstimates.xlsx",AdjWidth = T)

