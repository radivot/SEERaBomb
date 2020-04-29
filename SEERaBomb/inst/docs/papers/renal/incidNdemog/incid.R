source("renal/common/load.R")
source("renal/common/acros.R")
load("~/data/SEER/mrgd/popsae.RData")
(D=incidSEER(d,popsae,"ccRCC"))
(D=D%>%filter(age>=0,age<=85)%>%mutate(ageG=cut(age,seq(0,85,5))))
(D=D%>%group_by(sex,race,ageG)%>%
    summarize(age=mean(age),py=sum(py),n=sum(n))%>%mutate(incid=n/py))

D%>%ggplot(aes(x=age,y=incid,col=sex,shape=sex))+gxi+gyi+facet_grid(~race)+gp+
  sy+agts+jco+tc(13)+sbb+ltb+
  theme(legend.position=c(.9,.1),legend.key.height=unit(.7,'lines'))
ggsave("renal/outs/incidVsAge.pdf",width=4.5,height=3)

(D=incidSEER(d,popsae,"ccRCC"))
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
  theme(legend.position=c(.35,.91),legend.key.height=unit(.7,'lines'))
ggsave("renal/outs/StdIncidVsYear.pdf",width=4,height=3)



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

(D=incidSEER2(d,popsae,"ccRCC"))
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
ggsave("renal/outs/StdIncidVsReg.pdf",width=6,height=3)



