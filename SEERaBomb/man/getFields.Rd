\name{getFields}
\alias{getFields}
\title{ Get SEER data fields from SEER SAS file}
\description{ This function converts the SAS file in the SEER \file{incidence} directory into a data frame in R.}
\usage{getFields(seerHome="~/data/SEER")}

\arguments{
  \item{seerHome}{The directory that contains the SEER \file{population} and \file{incidence} directories.}
}
\details{SEER provides a SAS file for reading SEER ASCII data files into SAS. Unfortunately, no such file
is provided for R. This function bridges this gap by parsing the SAS file to automatically generate
a data frame that contains all of the SEER fields. This data frame describes these fields in 
terms of their names (short and long forms), their starting points, and their widths.}

\value{A data frame with one row for each field and columns that contain corresponding 
starting positions, widths, short names, and expansions thereof.}

\note{ This work was supported by the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. Maintenance is supported by the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package}, \link{mkSEER}, \link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
head(df,20)
}
}
\keyword{IO}
