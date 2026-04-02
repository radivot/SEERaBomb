\name{mkG2}
\alias{mkG2}
\title{ Make 2D GAM of SEER Mortality Data for 2 Causes of Death, Leukemic vs Other}
\description{Converts mortality rates in 5-year age groups into mgcv GAM fits.}
\usage{mkG2(seerHome="~/data/seer25",inDir="csvs",inFiles=c("cod4.txt","pops.txt"),
               outFiles=c("Glc.RData","Goc.RData","Gac.RData"),
               meanAgeFile="~/data/mrt/Ages.RData",
               vars=c("sex","age","COD","year","num"),
               popVars=c("sex","age","year","denom"))}

\arguments{
  \item{seerHome}{The directory that contains the SEER data csv file directory \file{inDir}. This should be writable by the user.}
  \item{inDir}{Subdirectory containing the \file{infiles}. }
  \item{inFiles}{Names of  SEER csv files placed in \file{inDir} subdirectory. }           
  \item{outFiles}{Names of R binaries containing the mgcv GAM fits for leukemic, other and all causes of death.}           
  \item{meanAgeFile}{Name of file containing mean ages of 5-year age groups across years and sexes.}           
  \item{vars}{Variable names in the numerator input file. }           
  \item{popVars}{Variable names in the denominator input file. }           
}

\details{The goal is create surface fits for two types of deaths.}
\value{None, it produces R binaries of 2D GAM of the SEER mortality data.}

%\note{.}
\author{ Tom Radivoyevitch (\email{radivot@gmail.com}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkCML} } }
\examples{
\dontrun{
library(SEERaBomb)
mkSEER(seerHome="~/data/seer25")  #for SEER data released in the Spring of 2025
}
}
\keyword{IO}
