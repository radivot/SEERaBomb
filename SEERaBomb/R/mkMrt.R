mkMrt<-function(username,passwd,country="USA",mrtHome="~/data/usMort"){
  mrtHome=path.expand(mrtHome)
  if(!dir.exists(mrtHome)) dir.create(mrtHome,recursive=T)
  mrt=demography::hmd.mx(country, username, passwd)$rate 
  names(mrt)=c("Female","Male","Total")
  save(mrt,file=f<-file.path(mrtHome,"mrt.RData"))
  cat("The dataframe mrt has been written to:",f,"\n")
}
