rm(list=ls()) 
library(SEERaBomb)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsa.RData")
# before you do anything else:

# 1) trim down columns to bare neccesities of columns for second cancer risk. 
head(canc)
canc=canc%>%select(-reg,-recno,-agerec,-numprims,-COD,-db,-age19)
head(canc)
head(popsa)
popsa=popsa%>%select(-db,-reg)
head(popsa)

# 2) add a trt column to define first cancer treatments of interest as a factor
canc$trt="noRad"
# canc$trt[canc$radiatn==0 |canc$radiatn==7] ="noRad"
canc$trt[canc$radiatn>0 &canc$radiatn<7] ="rad"
canc$trt=factor(canc$trt)
table(canc$trt)

# 3) modify the cancer field to include your specific cancer of interest
# canc$cancer=as.character(canc$cancer)
# canc$cancer[canc$cancer=="prostate" & canc$histo3 !=8140] ="prostateNonAdeno"
# canc$cancer=factor(canc$cancer)
# levels(canc$cancer)
# sort(table(canc$cancer),decr=T)

canc
popsa

head(canc) 
head(popsa) 
# we now pair these two objects

(pmSet=seerSet(canc,popsa,Sex="male")) #pooled (races) male seerSet
# pm=mk2D(pmSet) # 150 secs without writing files, 220 with writing
# system.time(save(pm,file="~/Results/pm.RData")) #indeed, writing this file takes 75 seconds
pm5=mk2D(pmSet,knots=5) # 5 knots take only 55 secs, but with L2D it is still big at 320 MB, vs 340MB for pm
# system.time(save(pm5,file="~/Results/pm5.RData")) #14 secs
sapply(pm5,class)
pm5$D

plot2D(pm5)

# plot2D(pm)  # most cancers look pretty good with either 5 or 10 knots. 
#NOTES: breastCIS weak here since males!  Liver dynamic is pretty. Lung spike in 2000 due to diffs in new registries
# melanoma on the rise, hump in NHL is odd (skin has hump in same spot??). Thyroid on rise, stomach still falling

# # check female fits
# plot2D(pf) 
# plot2D(pf5) 
# #APL and GI not trustworthy, otherCIS is very dynamic, but appears to be captured. note screening at 50 in rectal
# # rgl.quit() #seems I have to restart R if I want R to let go of my graphics card (which sucks my battery down fast)
# # rgl.close()

system.time(load("~/Results/pm5.RData")) # 1 secs to load. 
L=post1PYO(pm5$canc,brks=c(0,2,5),binIndx=1,Trt="rad" )
O=L$O
E=getE2(L$LPYM,pm5$D)
O/E
# EI=split(D,D$cancer) # expected incidences per 100,000 py, broken down by cancers
# debug(tsx2)
(BL=tsx2(pm5) )
str(BL)
names(BL)
# library(XLConnect)
names(BL[["noRad"]][["Obs"]]) 
rownames(BL[["noRad"]][["Obs"]][["(0,2]"]])
mkExcel(BL,pm5$bfn)


