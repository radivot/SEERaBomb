\name{mkAbomb}
\alias{mkAbomb}
\title{ Make an Abomb data SQLite database}
\description{ This function looks in AbombHome for the Abomb data files \file{lsshempy.csv} and \file{lssinc07.csv} 
and converts them into tables named \code{heme} and \code{solid}, 
respectively, in a an SQLite database file called \file{abomb.db}.}
\usage{mkAbomb(AbombHome="~/data/Abomb")}

\arguments{
  \item{AbombHome}{ The directory that contains the Abomb files.  This should be writable by the user.}
}

\details{This function makes an Abomb data SQLite database. Descriptions of column names are given 
in \code{hemeDesc} and \code{solidDesc}.}
\value{None. This function is called for its side-effect of producing an SQLite database file.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER},\link{mkSEER2},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
mkAbomb()  
}
}
\keyword{IO}
