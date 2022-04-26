\name{mkMrt}
\alias{mkMrt}
\title{Make mortality binaries}
\description{Gets mortality data from the Human Mortality Database \url{https://www.mortality.org/} 
and puts it in the file \file{mrt.RData}.}
\usage{ mkMrt(username,passwd,country="USA",mrtHome="~/data/usMort")}

\arguments{
  \item{username}{ Username of Human Mortality Database account.}
  \item{passwd}{ Password of Human Mortality Database account.}
  \item{country}{ This should probably stay at its default of USA.}
  \item{mrtHome}{ Directory that will contain the mortality data binary.  Should be writable by user.}
}

\value{None. This function is called for its side-effect of producing \file{mrt.RData}. }
\references{ Barbieri M, Wilmoth JR, Shkolnikov VM, et al. Data Resource Profile: 
             The Human Mortality Database (HMD). Int J Epidemiol. 2015;44: 1549-1556.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkMrt("username", "password")# sub in your personal account info
load("~/data/usMort/mrt.RData")
head(mrt$Female)
}
}
\keyword{IO}
