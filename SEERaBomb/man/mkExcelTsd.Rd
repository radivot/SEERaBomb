\name{mkExcelTsd}
\alias{mkExcelTsd}
\title{Make RR Excel file from tsd function output}
\description{Provides relative risks (RR) organized by 1st and 2nd cancers, times since 1st cancer diagnoses, and 1st cancer treatment. RR = O/E where O = observed cases and E = cases expected under a null hypothesis that prior cancers do not impact subsequent risks. If flip = FALSE (default), sheets = 1st cancers and rows = 2nd cancers, else sheets = 2nd cancers and rows = 1st cancers; columns are always intervals of years since diagnosis, in 
1st cancer treatment blocks. RR CI and observed numbers are included in each data cell.}
\usage{mkExcelTsd(seerSet,tsdn,outDir="~/Results",outName=NULL,flip=FALSE)} 
\arguments{
  \item{seerSet}{A seerSet list after it has been processed by tsd().  }
  \item{tsdn}{Name of set of times since diagnosis. This is based on the brks argument to tsd().
              If length >1 a brkst vector is assumed and coerced/collapsed to a tsdn string.}
  \item{outDir}{Folder of the Excel file that will be generated.}
  \item{outName}{if null (default), Excel file name = seerSet base file name (bfn) + tsdn,
                else it is outName. Eitherway, "Flipped" is appended to the name if flip is TRUE.  }
  \item{flip}{If FALSE, sheets are first cancers, rows seconds. If TRUE, sheets are second cancers, rows firsts.}
}

\value{ Returned invisibly, a list of data frames corresponding to sheets of the Excel file. 
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
pm=tsd(pm,brkst=mybrks,trts=c("noRad","rad")) 
(lab=paste0("b",paste(mybrks,collapse="_")))
(L=mkExcelTsd(pm,lab))
(L=mkExcelTsd(pm,lab,flip=TRUE))
}

}


\keyword{internal}
