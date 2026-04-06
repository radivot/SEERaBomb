\name{mkG6}
\alias{mkG6}
\title{ Make 2D GAM of SEER Mortality Data for 6 Causes of Death}
\description{Converts mortality rates in 5-year age groups into mgcv GAM fits.}
\usage{mkG6(seerHome="~/data/seer25",inFile="seerMrt.RData",outFile="G6.RData")}

\arguments{
  \item{seerHome}{The SEER data directory that contains \file{inFile}.}
  \item{inFile}{Name of binary made by mkSEERmrt(). }           
  \item{outFile}{Names of R binary containing the mgcv GAM fits for 6 different causes of death.} 
}

\details{The goal is to create surface fits for 6 types of deaths: Leukemic Causes (LC), 
CardioVascular disease (CV), Diabetes and Kidney disease (DK), other CAncers (CA),
INfections (IN) and Accidents, Suicides and Homocides (ASH).}
\value{None, it produces an R binary that contain a list G of six 2D GAMs of SEER mortality data. 
The binary also contains a list L of the six datasets used to produce the six GAM fits.}

%\note{.}
\author{Tom Radivoyevitch (\email{radivot@gmail.com}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkCML} } }
\examples{
\dontrun{
library(SEERaBomb)
mkG6()
}
}
\keyword{IO}
