foldD=function(D,keep="int") {
  D=D%>%group_by_at(vars(all_of(keep)))%>%summarize(O=sum(O),E=sum(E),PY=sum(PY),t=mean(t))
  D%>%mutate(EAR=(O-E)/PY,LL=EAR-1.96*sqrt(O)/PY,UL=EAR+1.96*sqrt(O)/PY,
             RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E)) 
}

