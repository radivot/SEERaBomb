\name{mkCML}
\alias{mkCML}
\title{ Make R binaries of SEER CML data.}
\description{Converts SEER*stat case-listing csv files of the SEER 8, 12 and 17 registries into one CML R binary file.}
\usage{mkCML(seerHome="~/data/seer25",inDir="csvs",inFiles=c("s8.txt",
                        "s12.txt","s17.txt"),outFile="cml.RData", vars=c("id","agedx",
                        "sex","race","yrdx","who","histo3","surv","COD","rad","chemo"))}

\arguments{
  \item{seerHome}{The directory that contains the SEER data csv file directory \file{inDir}. This should be writable by the user.}
  \item{inDir}{Subdirectory containing the three case-listing files listed in \file{infiles}. }
  \item{inFiles}{Names of thee SEER csv files placed in \file{inDir} subdirectory. }           
  \item{outFile}{Names of CML R binary data output file placed in \file{seerHome} }           
  \item{vars}{Variable names in the case-listing files. }           
}

\details{The goal is to speed up scripts by having them load in only CML data.}
\value{None, it produces an R binary file of the SEER data.}

%\note{.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
mkSEER(seerHome="~/data/seer25")  #for SEER data released in the Spring of 2025
}
}
\keyword{IO}
