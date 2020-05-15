source("uveal/common/setup.R")
library(WriteXLS)
library(cmprsk)
(d=d%>%select(yrdx,agedx,sex,surv,status,CODS))
# (D1=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),0,status)))
# fit=survfit(Surv(surv,status)~1,data=D1) 
# tail(fit$surv,1)
# head(d)
(D2=d%>%mutate(status=ifelse((status==1)&(!CODS%in%c("melanoma","eye")),2,status)))
fitc=with(D2,cuminc(surv,status))
labs=c("Death by Melanoma","Death by Other Causes")
# plot(fitc,curvlab=labs,xlab="Years Since Diagnosis",ylab="Cumulative Probability of Death")
# pdf("uveal/outs/cumIncEstimates.pdf",width=4,height=4)
# plot(fitc,curvlab=labs,xlab="Years Since Diagnosis",ylab="Cumulative Probability of Death",ylim=c(0,1))
# dev.off()
# str(fitc)
names(fitc)=c(" 1"," 2")  # gets rid of 1 at top
(g=ggcompetingrisks(fitc)+sbb+gx+ylab("Cumulative Probability of Death")+
  scale_color_manual(labels = labs, values = c("#F8766D", "#00BFC4")) +
  theme(legend.position = c(0.7, 0.2),legend.title=element_blank()) +labs(title="",caption=""))
ggsave("uveal/outs/cumIncPlotColor.pdf",width=4.5,height=3.5) #Fig. 3

# df=tibble(value=1:2,type=c("a","b"))
# (p <- ggplot(df, aes(x = value, y = value, col = type)) +
#   geom_point(size = 4))
# ggplot_build(p)$data
# 
# 1-tail(fitc[[1]]$est,1)  # 70% cure fraction

library("MALDIquant")

dmfs=as_tibble(fitc[[1]])
dos=as_tibble(fitc[[2]])
x0=seq(5,35,5)
(kos=match.closest(x0,dos$time))
(dos=dos%>%slice(kos)%>%mutate(CIos=paste0(sprintf('%.3f',est)," (",
                                           sprintf('%.3f',est-1.96*sqrt(var)),", ",
                                           sprintf('%.3f',est+1.96*sqrt(var)),")")))
(kmfs=match.closest(x0,dmfs$time))
(dmfs=dmfs%>%slice(kmfs)%>%mutate(CImfs=paste0(sprintf('%.3f',est)," (",
                                  sprintf('%.3f',est-1.96*sqrt(var)),", ",
                                  sprintf('%.3f',est+1.96*sqrt(var)),")")))

L=list(CumInc=tibble(Time=x0, "All Death Cumulative Incidence"=dos$CIos, #table 3
                              "Melanoma Death Cumulative Incidence"=dmfs$CImfs))
WriteXLS(L,ExcelFileName="uveal/outs/cumIncEstimates.xlsx",AdjWidth = T)

# (dos3=dos%>%slice(c(2,4,6)))
# paste0(dos3$CIos,", ",collapse="")
# (dmfs3=dmfs%>%slice(c(2,4,6)))
# paste0(dmfs3$CImfs,", ",collapse="")
# 
# 
# # young vs old diffs??
# Dy=D1%>%filter(agedx<60)
# fity=survfit(Surv(surv,status)~1,data=Dy) 
# tail(fity$surv,1)  #64%
# Do=D1%>%filter(agedx>=60)
# fito=survfit(Surv(surv,status)~1,data=Do) 
# tail(fito$surv,1)  #58%
# 
# fitc=with(D2%>%filter(agedx<60),cuminc(surv,status))
# 1-tail(fitc[[1]]$est,1)  # 68.3% cure fraction
# fitc=with(D2%>%filter(agedx>=60),cuminc(surv,status))
# 1-tail(fitc[[1]]$est,1)  # 70.4% cure fraction
# ##### nothting to write home about!
# # 
# # ggsurvplot(fity,Dy,legend.title="")
# # ggsurvplot(fito,Do,legend.title="")
# 
