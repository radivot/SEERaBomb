rad_noRad=function(DF) {# collapse chemo in trt and remove unknown rads
  trt=NULL
  DF$trt=fct_collapse(DF$trt, rad = c("rad.chemo", "rad.noChemo"),
                      noRad = c("noRad.chemo", "noRad.noChemo"),
                      unk = c("unk.chemo", "unk.noChemo") )
  DF%>%filter(trt!="unk")
  # df=as.data.table(DF) # namespace warning hassle, so skip data.table for now
  # df=df[trt!="unk"]
  # as_tibble(df)
}

