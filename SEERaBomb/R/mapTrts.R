mapTrts<-function(D){
  # adds a trt column to define cancer treatments of interest as a factor
  if ("radiatn"%in%names(D)&"chemo"%in%names(D)) {
    # this is how radiatn breaksdown. 0=none, 1=ExtBeam, 2+3=iso+implants, 4=extB+other
    # 5=NOS, 6=other rad (other=other than beam), 7=refused, 8+9=Unknown
    D$trt="noRad"  # will be left as 0 and 7. Do it this way to initialize the vector
    D$trt[D$radiatn%in%c(8,9)] ="unk"
    D$trt[D$radiatn>0 & D$radiatn<7] ="rad"
    D$trt=paste0(D$trt,c(".noChemo",".chemo")[D$chemo+1])
    D$trt=factor(D$trt,levels=c("rad.chemo","rad.noChemo","noRad.chemo","noRad.noChemo","unk.chemo","unk.noChemo"))
  } 
  if ("radiatn"%in%names(D)&!"chemo"%in%names(D)) {
    D$trt="noRad"  
    D$trt[D$radiatn%in%c(8,9)] ="unk"
    D$trt[D$radiatn>0 & D$radiatn<7] ="rad"
    D$trt=factor(D$trt,levels=c("rad","noRad","unk"))
  } 
  if (!"radiatn"%in%names(D)&"chemo"%in%names(D)) {
    D$trt="chemo"  
    D$trt[D$chemo==0] ="noChemo"
    D$trt=factor(D$trt,levels=c("chemo","noChemo"))
  } 
  if (!"radiatn"%in%names(D)&!"chemo"%in%names(D)) {
    D$trt="NA"  
    D$trt=factor(D$trt)
  }
  D
}  
