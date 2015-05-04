\name{getFields}
\alias{getFields}
\title{ Get fields from SEER SAS file}
\description{ Converts the SAS file in the SEER \file{incidence} directory into a data frame in R.}
\usage{getFields(seerHome="~/data/SEER")}

\arguments{
  \item{seerHome}{The directory that contains the SEER \file{population} and \file{incidence} directories.}
}
\details{SEER provides a SAS file for reading SEER ASCII data files into SAS.  This file is parsed by 
getFields() to generate
a data frame in R that contains all of the SEER fields. This data frame describes these fields in 
terms of their names (short and long forms), their starting points, and their widths.}

\value{A data frame with one row for each field and columns that contain corresponding 
starting positions, widths, short names, and expansions thereof.}

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
