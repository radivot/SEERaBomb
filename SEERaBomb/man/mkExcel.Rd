\name{mkExcel}
\alias{mkExcel}
\title{Makes an excel file that contains relative risks (RR) vs. time-since-diagnosis}
\description{This function outputs RR (O/E) where O is observed cases and E is cases expected under a null hypothesis that prior cancers do not impact subsequent risks.  The default of sheets/tabs = first cancers and rows = second cancers 
can be flipped if the argument flip is true; columns always change with time since first cancer diagnosis in blocks
of treatment types. RR CI and observed numbers are included in each data cell.}
\usage{mkExcel(seerSet,tsdn,outDir="~/Results", txt=NULL,flip=FALSE)} 
\arguments{
  \item{seerSet}{As made by tsx2.  }
  \item{tsdn}{Time since diagnosis set name. The seerSet base file name (bfn) + tsdn + txt will be the excel file name.}
  \item{outDir}{folder of the output file.}
  \item{txt}{If needed, additional text in file name to make it unique, e.g. for a modification in a
               cancer or treatment definition. }
  \item{flip}{If FALSE, sheets are first cancers, rows seconds. If TRUE, sheets are second cancers, rows firsts.}
}

\value{The output files are for a given sex with race typically pooled.
   Relative risks of second cancers are given versus tsd 
   and first cancers and first cancer therapies 
   (typically irradiated or not). A corresponding list of dataframes is returned invisibly. 
     }

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
library(SEERaBomb)
pm=simSeerSet()
pm=mk2D(pm) 
mybrks=c(0,0.8,1.2,3,4.8,5.2)
mybrks=c(0,1,3)
pm=tsd(pm,brks=mybrks,trts=c("rad")) 
(lab=paste0("b",paste(mybrks,collapse="_")))
pm
mkExcel(pm,lab,outDir="~/")
}


\keyword{IO}
