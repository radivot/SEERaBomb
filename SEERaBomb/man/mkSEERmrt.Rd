\name{mkSEERmrt}
\alias{mkSEERmrt}
\title{ Make SEER Mortality Data for all 111 Causes of Death}
\description{Merges SEER mortality and population csv files into one R binary file. }
\usage{mkSEERmrt(seerHome="~/data/seer26",inDir="csvs",inFile="codRate.txt",
               outFile="seerMrt.RData",
               meanAgeFile="~/data/mrt/Ages.RData",
               vars=c("COD","sex","a20","year","rate","num","denom"))}

\arguments{
  \item{seerHome}{The directory that contains the SEER data csv file directory \file{inDir}. This should be writable by the user.}
  \item{inDir}{Subdirectory containing the \file{infiles}. }
  \item{inFile}{Name of  SEER csv files placed in \file{inDir} subdirectory. }           
  \item{outFile}{Name of R binary ouput file.}           
  \item{meanAgeFile}{Name of file containing mean ages of 5-year age groups across years and sexes.}           
  \item{vars}{Variable names in the input file. }           
}

\details{The goal is to merge two csv files into one data frame.}
\value{None, it produces an R binary of the SEER mortality data.}

%\note{.}
\author{ Tom Radivoyevitch (\email{radivot@gmail.com}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkCML} } }
\examples{
\dontrun{
library(SEERaBomb)
mkSEERmrt(seerHome="~/data/seer25")  #for SEER data released in the Spring of 2025
}
}
\keyword{IO}
