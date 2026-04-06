\name{mkG2}
\alias{mkG2}
\title{ Make 2D GAM of SEER Mortality Data for 2 Causes of Death, Leukemic vs Other}
\description{Converts mortality rates in 5-year age groups into mgcv GAM fits.}
\usage{mkG2(seerHome="~/data/seer25",inFile="seerMrt.RData",
               outFiles=c("Glc.RData","Goc.RData","Gac.RData"))}

\arguments{
  \item{seerHome}{The directory that contains the SEER data csv file directory \file{inDir}. This should be writable by the user.}
  \item{inFile}{Name of binary made by mkSEERmrt(). }           
  \item{outFiles}{Names of R binaries containing the mgcv GAM fits for leukemic, other and all causes of death.} 
}

\details{The goal is create surface fits for two types of deaths, leukemic and other.}
\value{None, it produces R binaries of 2D GAM of the SEER mortality data.}

%\note{.}
\author{ Tom Radivoyevitch (\email{radivot@gmail.com}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkCML} } }
\examples{
\dontrun{
library(SEERaBomb)
mkG2()
}
}
\keyword{IO}
