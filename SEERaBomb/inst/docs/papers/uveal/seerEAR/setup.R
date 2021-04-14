#run first
graphics.off();rm(list=ls())#clear plots and environment 
library(tidyverse)
library(SEERaBomb);
library(WriteXLS)
library(survival);library(survminer)
library(bbmle); library(broom)
# load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
# d=canc%>%filter(primsite%in%c("C693","C694","C692"),histo3%in%8720:8790)
# d$cancer="uveal"
# d=d%>%filter(!is.na(surv))%>%mutate(status=as.numeric(COD>0),surv=(surv+0.5)/12)
# d5=d%>%select(yrdx,agedx,sex,surv,status)
# save(d,d5,file="uveal/data/d.RData")
setwd("~/case/active/seer/uveal/seerEAR")
load("data/d.RData")
load("data/mrtUSA.RData")#loads US mortality data
gp=geom_point(); gl=geom_line()
ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
ltp=theme(legend.position="top")
lh=theme(legend.direction="horizontal")
tc=function(sz) theme_classic(base_size=sz)
svts=scale_x_continuous(breaks=seq(0,35,5),limits=c(0,38))
gx=xlab("Years Since Diagnosis")
gy=ylab("Excess Absolute Risk of Mortality")
gh0=geom_hline(yintercept=0)

# tableD=function(D) {
#   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR),
#                          " (",sprintf('%.3f',LL),
#                          ", ",sprintf('%.3f',UL),")"))
#   D=D%>%mutate(Time=sprintf('%.2f',t))
#   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
#     select(Group,
#            Interval=int,
#            "Time in Years"=Time,
#            "Excess Absolute Risk (O-E)/PY"=CI,
#            "Deaths Observed"=O,
#            "Deaths Expected"=E,
#            "Person Years"=PY)
# }
# 
# # geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
# # agts=scale_x_continuous(breaks=c(25,50,75))#age times
# # 
# # 
# # tableDGS=function(D) {
# #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# #   D=D%>%mutate(Time=sprintf('%.2f',t))
# #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# #     select(Data,GBA,SL,Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # }
# # 
# # tableDG=function(D) {
# #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# #   D=D%>%mutate(Time=sprintf('%.2f',t))
# #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# #     select(Data,GBA,Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # }
# # 
# # tableG=function(D) {
# #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# #   D=D%>%mutate(Time=sprintf('%.2f',t))
# #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# #     select(GBA,Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # }
# # 
# # 
# # 
# # tableDS=function(D) {
# #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# #   D=D%>%mutate(Time=sprintf('%.2f',t))
# #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# #     select(Data,SL,Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # }
# # 
# # 
# # 
# # tableAll=function(D) {
# #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# #   D=D%>%mutate(Time=sprintf('%.2f',t))
# #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# #     select(Data,Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # }
# # 
# # 
# # # crashes
# # # tableCI=function(D,keep="Data") {
# # #   D=D%>%mutate(CI=paste0(sprintf('%.3f',EAR)," (",sprintf('%.3f',LL),", ",sprintf('%.3f',UL),")"))
# # #   D=D%>%mutate(Time=sprintf('%.2f',t))
# # #   D%>%mutate(E=sprintf('%.1f',E),PY=sprintf('%.1f',PY))%>%
# # #     select(vars(all_of(keep)),Interval=int,"Time in Years"=Time,"Excess Absolute Risk (O-E)/PY"=CI,
# # #            "Deaths Observed"=O,"Deaths Expected"=E,"Person Years"=PY)
# # # }
# # 
# # 
# # 
