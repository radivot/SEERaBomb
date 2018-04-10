\name{mkAbomb}
\alias{mkAbomb}
\title{Make Abomb Binaries}
\description{Converts Abomb files \file{lsshempy.csv} and \file{lssinc07.csv} 
into tibbles \code{heme} and \code{solid} in the file \file{abomb.RData}, which also contains descriptions of columns  
in tibbles \code{hemeDesc} and \code{solidDesc}.}
\usage{mkAbomb(AbombHome="~/data/abomb")}

\arguments{
  \item{AbombHome}{ Directory with Abomb files.  Should be writable by user.}
}

\details{ This function also creates an SQLite database \file{abomb.db} with tables \code{heme}, \code{solid},  
\code{hemeDesc} and \code{solidDesc}. }
\value{None. This function is called for its side-effect of producing the files \file{abomb.RData} and \file{abomb.db}. }
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkAbomb()
load("~/data/abomb/abomb.RData")
hemeDesc
heme
}
}
\keyword{IO}
