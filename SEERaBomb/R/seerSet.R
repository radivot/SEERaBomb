seerSet<-function(canc,popsa,ageStart=15,ageEnd=85,Sex="male", Race="pool") {
  # gimic to get rid of unwanted notes in R CMD check
  agedx=age=age86=cancer=yrdx=sex=race=surv=modx=yrbrth=NULL 
  
  if (!"age"%in%names(popsa)) { #assume first time here
    # so rename age86 as age to help plotting and joining later
    popsa=popsa%>%mutate(age=age86)%>%select(-age86) 
  }
  
  if (!"age"%in%names(canc)) { #assume  first time here
    # let year be yrdx as a whole integer to free it to become a real
    canc=canc%>%mutate(year=yrdx) 
#      canc$surv[canc$surv==0]=0.5   # push zero PY to 1 month to get empty py delivered at same times as 2nd cancers
#     canc=canc%>%mutate(surv=round(surv/12,3),yrdx=round(yrdx+modx/12,3))%>%     
    canc=canc%>%mutate(surv=round((surv+0.5)/12,3),yrdx=round(yrdx+(modx-0.5)/12,3))%>%    #modx=1=January 
      select(-modx)%>%
#       mutate(yrbrth=yrbrth+0.5,agedx=agedx+0.5) #convert birth years and ages at diagnosis to best guesses
      mutate(age=agedx+0.5) #convert birth years and ages at diagnosis to best guesses
#     canc=canc%>%mutate(age=agedx) 
    canc=canc%>%select(-agedx) 
    if ("age86"%in%names(canc)) canc=canc%>%select(-age86) 
  }  
  
#   canc=canc%>%filter(agedx>=(ageStart+0.5),agedx<(ageEnd+0.5),sex==Sex)
  canc=canc%>%filter(age>=(ageStart+0.5),age<(ageEnd+0.5),sex==Sex)
  popsa=popsa%>%filter(age>=ageStart,age<ageEnd,sex==Sex)
  if (Race!="pool") {
    canc=canc%>%filter(race==Race)
    popsa=popsa%>%filter(race==Race) 
  }
  canc$cancer=factor(canc$cancer) # get rid of any opposite sex cancer type levels
  cancerS=levels(canc$cancer)
  
  #   canc$race=factor(canc$race) # get rid of any removed race levels
  canc=canc%>%select(-sex,-race,-yrbrth)  
  # and package it all up
  seerSet=list(canc=canc,popsa=popsa,ageStart=ageStart,ageEnd=ageEnd,sex=Sex,race=Race,cancerS=cancerS,yearEnd=max(popsa$year))
  class(seerSet)="seerSet"
  seerSet
} # return a list that can be attached or with-ed in other functions
