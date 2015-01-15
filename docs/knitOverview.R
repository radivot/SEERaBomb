# note: to make a png from AI use export
# install.packages('knitr', dep=T)
#setwd("/Users/radivot/case/active/ccems/ccems/inst/doc")
#setwd("/Users/radivot/soft/rpacks/sbmlr/inst/doc")
#knit2pdf("quick-start.Rnw")         ## in 1 step
#knit2pdf("ccems.Rnw")         ## in 1 step
#purl("ccems.Rnw")
#purl("quick-start.Rnw")

require(knitr)
<<<<<<< HEAD
setwd("/Users/radivot/githubs/SEERaBomb/docs")
=======
setwd("/Users/radivot/github/SEERaBomb/docs")
>>>>>>> 3ab0ceb52bcb1d349d194bfb23210fb50cf274bd
knit2pdf("overview.Rnw",clean=TRUE)      
file.remove("overview.tex")
#purl("overview.Rnw")



