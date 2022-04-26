\name{mkAbomb}
\alias{mkAbomb}
\title{Make Abomb Binaries}
\description{Converts Abomb files \file{lsshempy.csv} and \file{lssinc07.csv} 
into tibbles \code{heme} and \code{solid} in the file \file{abomb.RData}.}
\usage{mkAbomb(AbombHome="~/data/abomb")}

\arguments{
  \item{AbombHome}{ Directory with Abomb files.  Should be writable by user.}
}

\details{  
The files \file{lsshempy.csv} and \file{lssinc07.csv} can be found under
The incidence of leukemia, lymphoma and multiple myeloma among atomic bomb survivors: 1950-2001 and Life Span Study Cancer Incidence Data, 1958-1998 of the Radiation Effects Research Foundation (RERF) website \url{https://www.rerf.or.jp/}.
}
\value{None. This function is called for its side-effect of producing \file{abomb.RData}. }
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkAbomb()
load("~/data/abomb/abomb.RData")
View(heme)
}
}
\keyword{IO}
