# NOTE: This script assumes that the SEER data is in ~/data/SEER 
.seerHome="~/data/SEER" 
rm(list=ls()) # note: dot variables defined above persist through such cleanings
# install.packages("RSQLite")
# install.packages("bbmle")
# install.packages("ggplot2")
library(RSQLite)
m=dbDriver("SQLite")
library(plyr)
library(bbmle)
library(ggplot2)

con=dbConnect(m,dbname=file.path(.seerHome,"00/all.db"))
dbListTables(con)
dbListFields(con,"pops")
dbListFields(con,"respir")
# pop=dbGetQuery(con,"SELECT * from pops where popage>6 and popage<19 and poprace=1")
#Note: whites only looked the same
pop=dbGetQuery(con,"SELECT * from pops where popage>6 and popage<19") 
head(pop)
(pop<-ddply(pop, .(popage,popsex), summarise,py=sum(population)))
head(pop,20)

d=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629 
and histO3=8140 and seqnum<2 and agerec>6 and agerec<19") # adenos
# and race=1 and histO3=8140 and seqnum<2 and agerec>6 and agerec<19")

# seems table column names are not case sensitive: o vs O in histO3


head(d)
(d<-ddply(d, .(agerec,sex), summarise,cases=length(agerec))) 
head(cbind(d,pop)) # just to see that they match up
d=cbind(d,py=pop[,"py"]) # only take the non-redundant py column
head(d)

d9=dbGetQuery(con,"SELECT * from respir where ICD9>=1620 and ICD9<=1629 
and histO3>=8070 and histO3<=8079 and seqnum<2 and agerec>6 and agerec<19") # squames
# and race=1 and  histO3>=8070 and histO3<=8079 and seqnum<2 and agerec>6 and agerec<19")
head(d9)

d9<-ddply(d9, .(agerec,sex), summarise,cases=length(agerec))
d9=cbind(d9,py=pop[,"py"])
head(d9)
head(d)

(d=cbind(rbind(d,d9),code=gl(2,dim(d9)[1],labels=c("adenocarcinoma","squamous cell"))) ) 
head(d)
d=transform(d,incid=1e6*cases/py)
d$sex=factor(d$sex,labels=c("Male","Female"))
age=c(0.5,3,seq(7.5,87.5,5))
d$age=age[d$agerec+1]
head(d,15)
names(d)[5]="Histology" # make legend start with capital

if(length(grep("linux",R.Version()$os))) windows <- function( ... ) X11( ... )
if(length(grep("darwin",R.Version()$os))) windows <- function( ... ) quartz( ... )
windows(height=4,width=8) 

(p <- ggplot(d,aes(x=age,y=incid,col=Histology))+geom_point(size=5)
 + labs(title="SEER Lung Cancer Incidence",x="Age (years)",
        y=expression(paste("Cases per ",10^6," Person-Years")))    
 + scale_y_log10(limits=c(1,1500)) )
(p=p + facet_grid(~ sex))
  mythem=theme( 
  plot.title = element_text(size = rel(1.5)),
  axis.title = element_text(size = rel(1.2)),
  axis.text = element_text(size = rel(1.2)),
  strip.text = element_text(size = rel(1.2))  )
p=p+mythem
(p=p+theme(legend.position = c(.33, .2),  #           legend.direction = 'vertical',
           legend.title = element_text(size = rel(1.1)) ,
           legend.text = element_text(size = rel(1.1))  ) )  

