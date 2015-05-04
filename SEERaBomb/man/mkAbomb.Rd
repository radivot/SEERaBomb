\name{mkAbomb}
\alias{mkAbomb}
\title{ Make Abomb SQLite database}
\description{ Converts Abomb files \file{lsshempy.csv} and \file{lssinc07.csv} 
into tables \code{heme} and \code{solid}, 
respectively, in SQLite database \file{abomb.db}.}
\usage{mkAbomb(AbombHome="~/data/Abomb")}

\arguments{
  \item{AbombHome}{ Directory with Abomb files.  Should be writable by user.}
}

\details{Descriptions of column names are given 
in \code{hemeDesc} and \code{solidDesc}.}
\value{None. This function is called for its side-effect of producing an SQLite database file.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER},\link{mkSEERold}} }
\examples{
\dontrun{
library(SEERaBomb)
mkAbomb()  
}
}
\keyword{IO}
