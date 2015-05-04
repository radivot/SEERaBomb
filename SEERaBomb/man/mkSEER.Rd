\name{mkSEER}
\alias{mkSEER}
\title{ Make R binaries of SEER data.}
\description{Converts SEER ASCII text files into large R binaries that include all cancer types and registries combined.}
\usage{mkSEER(df,seerHome="~/data/SEER",outDir="mrgd",outFile="cancDef",
                  indices = list(c("sex","race"), c("histo3","seqnum"),  "ICD9"),
                  writePops=TRUE,writeRData=TRUE,writeDB=TRUE)}

\arguments{
  \item{df}{ A data frame that was the output of \code{pickFields()}. This determines which fields to transfer. 
  Using the output of \code{getFields()} is a common mistake that must be avoided.}
  \item{seerHome}{ The directory that contains the SEER \file{population} and \file{incidence} directories.  This should be writable by the user.}
  \item{outDir}{seerHome subdirectory to write to. Default is mrgd for all registries merged together.}
  \item{outFile}{Base name of the SQLite database and cancer binary. Default = CancDef (Cancer Default). }           
  \item{indices}{Passed to \code{copy_to()} in \pkg{dplyr}. }           
  \item{writePops}{TRUE if you wish to write out the population data frame binaries. Doing so takes ~10 seconds, so
  savings of FALSE are small.}           
  \item{writeRData}{TRUE if you wish to write out the cancer data frame binary. Writing files takes most of the time. }           
  \item{writeDB}{TRUE if you wish to write cancer, popga, popsa, and popsae data frames to SQLite database tables.  }           
}

\details{This function uses the R package \pkg{LaF} to access the fixed-width format data files 
of SEER. \pkg{LaF} is fast, but it requires knowledge of all the widths of columns wanted, as well as the the widths of unwanted stretches in between. This knowledge is produced by \code{getFields()} and \code{pickFields()} combined. It is passed to \code{mkSEER} as the argument \code{df}. \code{mkSEER} uses \code{dplyr} objects of class \code{tbl_df}. 
}
\value{None, it produces R binary files of the SEER data.}

\note{ This takes a substantial amount of RAM (it works on a Mac with 16 GB of RAM) and time (~3 minutes
using default fields).}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
(df=pickFields(df))
# the following will take a several minutes, but may only need 
# to be done roughly once per year, with each release.
mkSEER(df)
}
}
\keyword{IO}
