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
    #and convert birth years and ages at diagnosis to best guesses
    canc=canc%>%mutate(surv=round(surv/12,3),yrdx=round(yrdx+modx/12,3))%>%     
      select(-modx)%>%
      mutate(yrbrth=yrbrth+0.5,agedx=agedx+0.5)
    canc=canc%>%mutate(age=agedx)%>%select(-age86) 
  }  
  
  canc=canc%>%filter(agedx>=(ageStart+0.5),agedx<(ageEnd+0.5),sex==Sex)
  popsa=popsa%>%filter(age>=ageStart,age<ageEnd,sex==Sex)
  if (Race!="pool") {
    canc=canc%>%filter(race==Race)
    popsa=popsa%>%filter(race==Race) 
  }
  canc$cancer=factor(canc$cancer) # get rid of any opposite sex cancer type levels
  cancerS=levels(canc$cancer)
  canc$race=factor(canc$race) # get rid of any removed race levels
  
  # and package it all up
  seerSet=list(canc=canc,popsa=popsa,ageStart=ageStart,ageEnd=ageEnd,sex=Sex,race=Race,cancerS=cancerS)
  class(seerSet)="seerSet"
  seerSet
} # return a list that can be attached or with-ed in other functions
