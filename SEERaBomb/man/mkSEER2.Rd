\name{mkSEER2}
\alias{mkSEER2}
\title{ Make SEER binaries Version 2}
\description{Converts SEER ASCII text files into large R binaries with cancers and registries pooled.}
\usage{mkSEER2(df,seerHome="~/data/SEER",outDir="mrgd",outFile="cancDef",
                  indices = list(c("sex","race"), c("histo3","seqnum"),  "ICD9"),
                  writePops=TRUE,writeRData=TRUE,writeDB=TRUE)}

\arguments{
  \item{df}{ A data frame that was the output of \code{pickFields}. This determines which fields to transfer. }
  \item{seerHome}{ The directory that contains the SEER \file{population} and \file{incidence} directories.  This should be writable by the user.}
  \item{outDir}{seerHome subdirectory to write to. Default is all for all registries.}
  \item{outFile}{Base name of the SQLite database and cancer binary. Default is all for all cancers. }           
  \item{indices}{This is the argument \code{indexes} of \code{copy_to()} in \pkg{dplyr}. }           
  \item{writePops}{TRUE if you wish to write out the population data.frame binaries. Doing so takes ~13 seconds, so the
  savings of setting this to FALSE are quite small.}           
  \item{writeRData}{TRUE if you wish to write out the cancer data.frame binary. Writing the files takes most of the time. }           
  \item{writeDB}{TRUE if you wish to write the cancer, popga, and popsa data.frames to SQLite database tables.  }           
}

\details{This function uses the R package \pkg{LaF} to access the fixed-width format data files 
of SEER. \pkg{LaF} is fast, but it requires knowledge of all the widths of columns wanted, as well as the the widths of unwanted stretches in between. This knowledge is produced by \code{getFields()} and \code{pickFields()} combined. It is passed to \code{mkSEER2} as the argument \code{df}. Whereas \code{mkSEER} uses standard R data.frames, \code{mkSEER2} uses \code{dplyr} objects of class \code{tbl_df}. Whereas \code{pool} does not index the SQLite db it produces, \code{mkSEER2} does, with \code{indices = list(c("sex","race"), "histo3", "ICD9")}.
}
\value{None. This function is called for its side-effects: it produces R binary data files.}

\note{ This takes a substantial amount of RAM (it works on a Mac with 16 GB of RAM) and time (~3 minutes
using default fields and ~8 minutes using all fields). Development of this function was supported by the Cleveland Clinic Foundation. }
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{getFields},\link{pickFields}} }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
(df=pickFields(df))
# the following will take a several minutes, but may only need 
# to be done roughly once per year, with each release.
for (i in c("73","92","00")) mkSEER(df,dataset=i)
}
}
\keyword{IO}
