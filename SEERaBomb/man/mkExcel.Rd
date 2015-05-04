\name{mkExcel}
\alias{mkExcel}
\title{Make RR Excel file}
\description{Provides relative risks (RR) organized by 1st and 2nd cancers, times since 1st cancer diagnoses (tsd), and 1st cancer treatment. RR = O/E where O = observed cases and E = cases expected under a null hypothesis that prior cancers do not impact subsequent risks. If flip = FALSE (default), sheets = 1st cancers and rows = 2nd cancers, else sheets = 2nd cancers and rows = 1st cancers; columns are always tsd in blocks
of 1st cancer treatment types. RR CI and observed numbers are included in each data cell.}
\usage{mkExcel(seerSet,tsdn,outDir="~/Results",txt=NULL,flip=FALSE)} 
\arguments{
  \item{seerSet}{A seerSet list after it has been processed by tsd().  }
  \item{tsdn}{Name of set of times since diagnosis. This is based on the brks argument to tsd().}
  \item{outDir}{Folder of the Excel file that will be generated.}
  \item{txt}{Additional text, if needed to make the Excel file name unique.  }
  \item{flip}{If FALSE, sheets are first cancers, rows seconds. If TRUE, sheets are second cancers, rows firsts.}
}

\value{ Returned invisibly, a list of data frames corresponding to sheets of the Excel file. 
  Excel file name = seerSet base file name (bfn) + tsdn + txt.
     }

\note{ Outputs are for a given sex. Races are typically pooled.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
library(SEERaBomb)
pm=simSeerSet()
pm=mk2D(pm) 
mybrks=c(0,1,5,10)
pm=tsd(pm,brks=mybrks,trts=c("noRad","rad")) 
(lab=paste0("b",paste(mybrks,collapse="_")))
(L=mkExcel(pm,lab))
(L=mkExcel(pm,lab,flip=TRUE))
}

}


\keyword{IO}
