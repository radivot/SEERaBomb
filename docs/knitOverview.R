# note: to make a png from AI use export
# install.packages('knitr', dep=T)
#setwd("/Users/radivot/case/active/ccems/ccems/inst/doc")
#setwd("/Users/radivot/soft/rpacks/sbmlr/inst/doc")
#knit2pdf("quick-start.Rnw")         ## in 1 step
#knit2pdf("ccems.Rnw")         ## in 1 step
#purl("ccems.Rnw")
#purl("quick-start.Rnw")

require(knitr)
setwd("/Users/radivot/githubs/SEERaBomb/docs")
knit2pdf("overview.Rnw",clean=TRUE)      
file.remove("overview.tex")
#purl("overview.Rnw")



