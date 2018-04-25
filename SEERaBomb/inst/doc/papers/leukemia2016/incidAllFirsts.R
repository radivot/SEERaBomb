# incidAllFirsts.R (Figure 2)
rm(list=ls()) 
library(SEERaBomb)
library(ggplot2)
library(dplyr)
load("~/data/SEER/mrgd/cancDef.RData")
load("~/data/SEER/mrgd/popsae.RData") 
HM=c("AML","MDS","CMML","CML","MPN","ALL","CLL","SLL","HCL","OL","NHL","MM","HL","LGL")

graphics.off()
quartz(height=4.5,width=7) 
# theme_set(theme_bw())
theme_update(legend.position = c(.85, .21),
             axis.text = element_text(size = rel(1.5)),
             plot.title = element_text(size = rel(1.7),hjust = 0.5),
             axis.title = element_text(size = rel(1.5)),
             legend.text = element_text(size = rel(1.5)),
             legend.title = element_text(size = rel(1.5))      )

for (HSC in c(FALSE,TRUE)) {
  if (HSC)
    m=canc%>%filter(cancer%in%HM)%>%mutate(age=agedx+0.5)%>%
      group_by(sex,age)%>%summarise(cases=n()) else
        m=canc%>%filter(!cancer%in%HM)%>%mutate(age=agedx+0.5)%>%
          group_by(sex,age)%>%summarise(cases=n()) 
      
      pops=popsae%>%group_by(sex,age)%>%summarise(py=sum(py))
      head(m)
      s=data.frame(sex=sort(unique(m$sex)))
      pL=left_join(s,pops)
      head(pL)
      d=left_join(pL,m)
      d=d%>%mutate(py=py/1e5,incid=cases/py)
      head(d)
      names(d)[1]="Sex"
      levels(d$Sex)=c("Male","Female")
      g=qplot(age,incid,col=Sex,data=d,
              main=ifelse(HSC,"All Hematologic Cancers Combined","All Non-Hematologic Cancers Combined"),
              ylab="Cases/100,000 Person-Years",
              xlab="Age",log="y")+geom_line() 
      # g=g+ggtitle(ifelse(HSC,"Hematological Cancers","Non-Hematological Cancers"))
      # g = g + scale_color_grey(start = 0, end = 0.6)
      if (HSC) {brks=c(0,25,50,75,100)} else{
        g=g+ geom_vline(xintercept = c(40,50,65),col="gray")
        brks=c(0,25,40,50,65,75,100)
      }
      g=g+scale_x_continuous(breaks=brks,labels=brks)
      print(g)
      if (HSC) ggsave("~/Results/amlMDS/allCancersCombVsAgeHSC.pdf") else
        ggsave("~/Results/amlMDS/allCancersCombVsAge.pdf")
      if (HSC) ggsave("~/Results/amlMDS/allCancersCombVsAgeHSC.png") else
        ggsave("~/Results/amlMDS/allCancersCombVsAge.png")
}
d=d%>%group_by(age)%>%mutate(r=incid[1]/incid[2])
head(d)
d$r
mean(d$r)
