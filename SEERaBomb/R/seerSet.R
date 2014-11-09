seerSet<-function(canc,popsa,ageStart=15,ageEnd=85,Sex="male", Race="pool", picks=NULL) {
  # gimic to get rid of unwanted notes in R CMD check
  agedx=age86=cancer=yrdx=sex=race=surv=modx=yrbrth=NULL 
  #   require(dplyr)
  canc=canc%>%filter(agedx>=ageStart,agedx<ageEnd,sex==Sex)
  popsa=popsa%>%filter(age86>=ageStart,age86<ageEnd,sex==Sex)
  if (Race!="pool") {
    canc=canc%>%filter(race==Race)
    popsa=popsa%>%filter(race==Race) 
  }
  canc$cancer=factor(canc$cancer) # get rid of any opposite sex cancer type levels
  if (is.null(picks)) picks=levels(canc$cancer) else { 
    canc=canc%>%filter(cancer%in%picks)
    canc$cancer=factor(canc$cancer) # get rid of any removed cancer type levels
  }
  canc$race=factor(canc$race) # get rid of any removed race levels
  # do some mutations to help plotting and joining later
  canc=canc%>%mutate(year=yrdx) 
  popsa=popsa%>%mutate(age=age86)%>%select(-age86) 
  
  #and convert birth years and ages at diagnosis to best guesses
  canc=canc%>%mutate(surv=round(surv/12,3),yrdx=round(yrdx+modx/12,3))%>%     
    select(-modx)%>%
    mutate(yrbrth=yrbrth+0.5,agedx=agedx+0.5)

  canc=canc%>%mutate(age=agedx)%>%select(-age86) 
  
  
  # and package it all up
  seerSet=list(canc=canc,popsa=popsa,ageStart=ageStart,ageEnd=ageEnd,sex=Sex,race=Race,picks=picks)
  class(seerSet)="seerSet"
  seerSet
} # return a list that can be attached or with-ed in other functions
