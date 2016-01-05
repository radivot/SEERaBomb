rm(list=ls()) 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsa.RData")
# before you do anything else:

# 1) trim down columns to bare neccesities of columns for second cancer risk. 
head(canc)
# canc=canc%>%select(-reg,-recno,-agerec,-numprims,-COD,-db,-age19)
canc=canc%>%select(-reg,-COD,-db)
head(canc)
head(popsa)
popsa=popsa%>%select(-db,-reg)
head(popsa)

# # 2) add a trt column to define first cancer treatments of interest as a factor
# canc$trt="noRad"
# # canc$trt[canc$radiatn==0 |canc$radiatn==7] ="noRad"
# canc$trt[canc$radiatn>0 &canc$radiatn<7] ="rad"
# canc$trt=factor(canc$trt)
# table(canc$trt)

# 3) if needed, modify the cancer field to have your specific cancer of interest.
# To check current definitions look at the source of mapCancs
mapCancs   
# so if you don't like prostate as ICD9=185 because you only want adeno types, you could do 
# canc$cancer=as.character(canc$cancer)
# canc$cancer[canc$cancer=="prostate" & canc$histo3 !=8140] ="prostateNonAdeno"
# canc$cancer=factor(canc$cancer)
# levels(canc$cancer)
# sort(table(canc$cancer),decr=T)
head(canc) 
head(popsa) 
# we now pair these two objects
(pm=seerSet(canc,popsa,Sex="male")) #pooled (races) male seerSet
# pm=mk2D(pm,secondS=c("AML","MDS")) # 5 knots take only 55 secs, but with L2D it is still big at 320 MB, vs 340MB for pm
pm=mk2D(pm) # 5 knots take only 55 secs, but with L2D it is still big at 320 MB, vs 340MB for pm

# plot2D(pm5)
# NOTES: breastCIS weak here since males!  Liver dynamic is pretty. Lung spike in 2000 due to diffs in new registries
# melanoma on the rise, hump in NHL is odd (skin has hump in same spot??). Thyroid on rise, stomach still falling

# # check female fits
# (pfSet=seerSet(canc,popsa,Sex="female")) #pooled (races) female seerSet
# pf=mk2D(pfSet) 
# plot2D(pf) 
# #APL and GI not trustworthy, otherCIS is very dynamic, but appears to be captured. note screening at 50 in rectal
# # rgl.quit() #seems I have to restart R if I want R to let go of my graphics card (which sucks my battery down fast)
# # rgl.close()
# 
# system.time(save(pm5,file="~/Results/pm5.RData")) # 10 secs to save. 
# 
# rm(list=ls()) 
# library(SEERaBomb)
# system.time(load("~/Results/pm5.RData")) # 1 secs to load. 
# debug(tsd)
# debug(mkPYMR)
# undebug(tsd)
# debug(post1PYO)

L=post1PYO(pm5$canc,brks=c(0,2,5),binIndx=1,Trt="rad",yearEnd=2012 )
names(L)
(E=with(pm5,getE(L$LPYM,D,ageStart,ageEnd,yearEnd,cancerS,secondS)))

brks=c(0,0.25,0.5,0.75,1,1.5,2,2.5,3,4,5,7,10,13,16,20)
pm=tsd(pm5,brks=brks,trts=c("rad","noRad")) 
(lab=paste0("b",paste(brks,collapse="_")))
mkExcel(pm,lab)
mkExcel(pm,lab,flip=TRUE)


