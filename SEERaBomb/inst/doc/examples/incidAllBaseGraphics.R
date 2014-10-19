# This is very old code, so don't try to learn from it. Just run it and note on 
# a mac that command~ toggles you through the quartz windows of plots. Also note
# that incidAllgg.R does similar things using modern code/ggplot2; the advantage
# of the script here is that it spreads things out over many large figures, and
# shows race differences.

filecanc=c('breast','digothr','malegen','femgen','other','respir','colrect','lymyleuk','urinary')
load("~/data/SEER/00/pops.RData") # this loads in pops
pyf=pym=vector(3,mode="list"); 
for (i in 0:18) { for (r in 1:2) {
		pym[[r]][i+1]=with(pops,sum(population[(popsex==1)&(popage==i)&(poprace==r)]))
		pyf[[r]][i+1]=with(pops,sum(population[(popsex==2)&(popage==i)&(poprace==r)]))}
	pym[[3]][i+1]=with(pops,sum(population[(popsex==1)&(popage==i)&(poprace>2)]))
	pyf[[3]][i+1]=with(pops,sum(population[(popsex==2)&(popage==i)&(poprace>2)])) }

if(length(grep("linux",R.Version()$os))) windows <- function( ... ) X11( ... )
if(length(grep("darwin",R.Version()$os))) windows <- function( ... ) quartz( ... )


graphics.off();
for (k in c(1:9))
{
	load(paste("~/data/SEER/00/",filecanc[k],".RData",sep="")) # this loads in DF
	DF=DF[!is.na(DF$ICD9),]  # get rid of missing ICD9 entries
	if (k==1) { canc=c('Breast'); code=list(c(1741,1748)) } 
	if (k==2) {canc=c('Esophogeal','Stomach','Liver','Pancreatic'); 
		code=list(c(1504,1505),c(1510,1519),c(1550,1550),c(1570,1579)) }
	if (k==3) {canc=c("Prostate"); code=list(c(185,185))}
	if (k==4) {canc=c("Cervical","Uterine","Vaginal"); 
		code=list(c(1809,1809),c(1820,1820),c(1830,1830))}
	if (k==5) {canc=c("Thyroid","Brain"); code=list(c(193,193),c(1911,1919))}
	if (k==6){canc=c("Laryngeal","Lung");code=list(c(1610,1610),c(1622,1629))}
	if (k==7){canc=c("Colorectal"); code=list(c(1530,1539)) }
	if (k==8) { canc=c("NHL",   "Hodgkins",   "MML"         ,"ALL","     CLL",       "AML",       "CML")
		code=list(c(2000,2008),c(2015,2016),c(2030,2030),c(2040,2040),c(2041,2041),c(2050,2050),c(2051,2051))}
	if (k==9) { canc=c("Bladder","Kidney"); code=list(c(1880,1889),c(1890,1891))}
	for (j in 1:length(code))
	{	
		windows(width=12,height=5)
		par(mfrow=c(1,3),mar=c(4.7,0,2,0),oma=c(0,7.1,4,0),lwd=3,cex.lab=2.8,cex.axis=2.5,cex.main=2.8)
		Indx=(DF$ICD9>=code[[j]][1])&(DF$ICD9<=code[[j]][2])&(DF$numprims==1) 
		incdf=incdm=vector(3,mode="list"); 
		casesf=casesm=vector(3,mode="list"); 
		for (i in 1:3) {
			if (i==3) d=DF[Indx&(DF$race>2)&(DF$race<98),] else 
				d=DF[Indx&(DF$race==i),] 
			if ((k!=1)&(k!=4)) incdm[[i]]=hist(d$agerec[d$sex==1],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts/pym[[i]]
			if (k!=3) incdf[[i]]=hist(d$agerec[d$sex==2],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts/pyf[[i]]
			if ((k!=1)&(k!=4)) casesm[[i]]=hist(d$agerec[d$sex==1],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts
			if (k!=3) casesf[[i]]=hist(d$agerec[d$sex==2],breaks=c(seq(-.5,17.5,1),100),plot=FALSE)$counts
		}	
		if ((k!=1)&(k!=3)&(k!=4))  # both sexes 
			df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),6),race=rep(c("white","black","asian"),each=19,times=2),
					sex=factor(rep(c("male","female"),each=57),levels=c("male","female")),
					incd=c(unlist(incdm),unlist(incdf)),cases=c(unlist(casesm),unlist(casesf)) ,py=c(unlist(pym),unlist(pyf)) )
		if ((k==1)|(k==4))  df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),3),race=rep(c("white","black","asian"),each=19),
					sex=factor(rep("female",each=57),levels=c("male","female")),incd=unlist(incdf),
					cases=unlist(casesf),py=unlist(pyf)  )
		if (k==3) 	df=data.frame(age=rep(c(0.5,3,seq(7.5,87.5,5)),3),race=rep(c("white","black","asian"),each=19),
					sex=factor(rep("male",each=57),levels=c("male","female")),incd=unlist(incdm),
					cases=unlist(casesm),py=unlist(pym)  )
		df=subset(df,(age>20)&(incd>0))
#		lapply(df,class)
		rng=range(df$incd)
		for (i in 1:3) {
			di=subset(df,race==c("white","black","asian")[i])
#			summary(lmf<-glm(cases~age+sex+offset(log(py)),family=poisson,data=di))
			with(di,plot(age,incd,log="y",xlab="Age",type='p',col=c("blue","red")[as.numeric(sex)],pch=as.numeric(sex),
							ylab="",cex=2,yaxt="n",ylim=rng))
			if (i==1) {axis(side=2,las=1, at=c(1e-6,1e-5,1e-4,1e-3,1e-2),labels=expression(1,10,10^2,10^3,10^4))
				mtext(expression(paste("Cases per ",10^6," Person-Years")),side=2,line=3.8,cex=2)}
			mtext(paste(sum(di$cases),c("whites","blacks","others")[i]),side=3,line=.5,cex=1.5,adj=0.5)
			legend("bottomright",c("Males","Females"),col=c("blue","red"),pch=1:2,text.col=c("blue","red"),bty="n",cex=2)
		} # i loop over races
		title(paste("SEER",canc[j],"Cancer Incidence 2000-2010"),cex=3,outer=T)
	} # j loop over cancers within file
}  # k loop on k over files

