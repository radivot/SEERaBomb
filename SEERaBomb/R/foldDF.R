foldDF=function(DF,keep=c("cancer1","cancer2","int","trt")) { #sum over sex
  O=E=RR=all_of=NULL
  DF=DF%>%group_by_at(vars(all_of(keep)))%>%summarize(O=sum(O),E=sum(E),t=mean(t))
  DF%>%mutate(RR=O/E,rrL=qchisq(.025,2*O)/(2*E),rrU=qchisq(.975,2*O+2)/(2*E))%>%filter(E>0)%>%ungroup 
}

