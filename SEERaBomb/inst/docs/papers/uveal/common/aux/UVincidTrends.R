graphics.off();rm(list=ls())#clear plots and environment
library(tidyverse);library(SEERaBomb)
# load("~/data/SEER/mrgd/cancPrim.RData")#load SEER cancer data
# (codes=paste0("C",692:694))
# d=canc%>%filter(primsite%in%codes,histo3%in%8720:8790)  # 119+1525+9035
# d$cancer="uveal"
# table(d$CODS)
# head(d)
# d=d%>%select(-primsite,-histo3,-ICD9,-CODS)
# save(d,file="uveal/data/uveal2019.RData")

gp=geom_point()
agts=scale_x_continuous(breaks=c(25,50,75))#age times
sbb=theme(strip.background=element_blank())
ltb=theme(legend.margin=margin(0,0,0,0),legend.title=element_blank())
library(ggsci)
jco=scale_color_jco()
tc=function(sz) theme_classic(base_size=sz);
gxi=xlab("Age (Years)")
gyi=ylab(quote(paste("Cases per ",10^5," Person Years")))
sy=scale_y_log10()



library(bbmle)

load("uveal/data/uveal2019.RData")
load("~/data/SEER/mrgd/popsae.RData")
(D=incidSEER(d,popsae,"uveal"))
# (D=D%>%filter(age>=10,age<=80,year>=2000)%>%mutate(ageG=cut(age,seq(10,80,5))))
(D=D%>%filter(age>=0,age<=85)%>%mutate(ageG=cut(age,seq(0,85,5))))
(D=D%>%group_by(sex,race,ageG)%>%
    summarize(age=mean(age),py=sum(py),n=sum(n))%>%mutate(incid=n/py))
# summary(mm1<-mle2(n~dpois(lambda=exp(c+k*(age-50))*py),
#                   parameters=list(c~race,k~race),#kW-O P=7e-8
#                   method="Nelder-Mead",start=list(c=-1,k=0.04),data=D)) 
# D$EI=predict(mm1)/D$py#same as D=D%>%ungroup()%>%mutate(EI=predict(mm1)/py) 

D%>%ggplot(aes(x=age,y=incid,col=sex,shape=sex))+gxi+gyi+facet_grid(~race)+gp+
  # geom_line(aes(y=EI))+
  # scale_y_log10(breaks=c(1,2,5))+
  sy+agts+jco+tc(13)+sbb+ltb+
  theme(legend.position=c(.75,.91),legend.key.height=unit(.7,'lines'))
ggsave("uveal/outs/incidVsAge.pdf",width=4,height=3)


(D=incidSEER(d,popsae,"uveal"))
range(D$age)
stdUS=stdUS%>%filter(age<99)
(D=D%>%filter(age<99))
(D=D%>%group_by(sex,year,age)%>%
    summarize(py=sum(py),n=sum(n))%>%mutate(incid=n/py))
D=left_join(D,stdUS)
D=D%>%summarize(Incid=sum(incid*prop))
gl=geom_line()
D%>%ggplot(aes(x=year,y=Incid,col=sex,shape=sex))+gxi+gyi+#gp+
  jco+tc(13)+ltb+gl+
  theme(legend.position=c(.75,.91),legend.key.height=unit(.7,'lines'))
ggsave("uveal/outs/StdIncidVsYear.pdf",width=4,height=3)


# geRR=geom_errorbar(aes(ymin=rrL,ymax=rrU),width=.2)
# ge=geom_errorbar(aes(ymin=LL,ymax=UL),width=0.2)#for absolute risks
# gh=geom_hline(yintercept=1)
# svts=scale_x_continuous(breaks=c(0,5,10))#surv times
# ltp=theme(legend.position="top")
# lh=theme(legend.direction="horizontal")


incidSEER2=function(canc, popsae, cancers) 
{
  cancer = sex = race = agedx = yrdx = year = age = py = cases = NULL
  startYrs = c(CMML = 1993, MDS = 2001, NOS = 2001, RA = 2001, 
               RAEB = 2001, RARS = 2001, Del5q = 2001, LGL = 2010)
  (startYrs = c(startYrs, MPN = 2001, unknown = 2001, AMLti = 2001, 
                LGL = 2010))
  (outnms = setdiff(cancers, names(startYrs)))
  x = rep(1975, length(outnms))
  names(x) <- outnms
  (startYrs = c(startYrs, x))
  D = canc %>% select(cancer, sex, race, agedx, reg,year = yrdx) %>% 
    mutate(age = agedx + 0.5) %>% filter(cancer %in% cancers) %>% 
    select(-agedx)
  m = D %>% group_by(cancer, sex, race, age, year,reg) %>% summarise(n = n())
  p = popsae %>% group_by(sex, race, age, year,reg) %>% summarise(py = sum(py))
  s = data.frame(sex = sort(unique(m$sex)))
  c = data.frame(cancer = cancers)
  cs = merge(c, s) %>% arrange(cancer, sex)
  options(warn = -1)
  pL = left_join(cs, p, by = "sex")
  d = left_join(pL, m)
  options(warn = 0)
  d[is.na(d$n), "n"] = 0
  d = d %>% group_by(cancer) %>% filter(year >= startYrs[as.character(cancer[1])])
  d %>% mutate(py = py/1e+05, incid = n/py)
}

(D=incidSEER2(d,popsae,"uveal"))
range(D$age)
stdUS=stdUS%>%filter(age<99)
(D=D%>%filter(age<99))
(D=D%>%group_by(sex,reg,age)%>%
    summarize(py=sum(py),n=sum(n))%>%mutate(incid=n/py))
D=left_join(D,stdUS)
D=D%>%summarize(Incid=sum(incid*prop))
D%>%ggplot(aes(x=reg,y=Incid,col=sex,shape=sex))+xlab("Registry")+gyi+#gp+
  jco+tc(13)+ltb+gp+
  theme(legend.position="bottom",legend.key.height=unit(.7,'lines'))
  # theme(legend.position=c(.75,.91),legend.key.height=unit(.7,'lines'))
ggsave("uveal/outs/StdIncidVsReg.pdf",width=6,height=3)



