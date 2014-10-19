\name{mkSEER}
\alias{mkSEER}
\title{ Make SEER data binaries }
\description{ This function converts large SEER ASCII text data files into smaller R binaries.}
\usage{mkSEER(df,seerHome="~/data/SEER",
            dataset=c("00","73","92"),SQL=TRUE, mkDFs=FALSE)}

\arguments{
  \item{df}{ A data frame that was the output of \code{pickFields}. This determines which fields to transfer. }
  \item{seerHome}{ The directory that contains the SEER \file{population} and \file{incidence} directories.  This should be writable by the user.}
  \item{dataset}{The SEER dataset to use, specified as a string of the last two digits of the starting year, i.e. \code{"73"} = 1973-2011 = SEER 9, \code{"92"}  = 1992-2011 = SEER 13, and \code{"00"} = 2000-2011 = SEER 18, where in SEER X, X is the number of SEER registries. This determines the subdirectory in \file{incidence} and \file{population} from which data is taken and is also the name of the subdirectory of \code{seerHome} to which the R data files will be written, e.g. \code{"00"} processes SEER18 data (collected since 2000) and places the binary results in /data/SEER/00.}
  \item{SQL}{TRUE if an SQLite database is to be created and populated with tables. The single file \file{all.db} produced in this case can be significantly larger than the sum of the \file{*.RData} files also produced. }           
  \item{mkDFs}{TRUE if you wish to make data.frame binaries of the cancer.txt files. }           
}

\details{This function uses the R package \pkg{LaF} to access the fixed-width format data files 
of SEER. \pkg{LaF} is fast, but it requires knowledge of all the widths of columns wanted, as well as the the widths of unwanted stretches in between. This knowledge is produced by \code{getFields()} and \code{pickFields()} combined. It is passed to \code{mkSEER} as the argument \code{df}.
}
\value{None. This function is called for its side-effects: it produces R binary data files.}

\note{ This version of \pkg{SEERaBomb} works only with the SEER data release of April 2014.
Using the default field choices of \code{pickFields()},
computing times range from ~10 seconds for 1992-2011 processing without SQLite database generation, to ~90 seconds for 1973-2011 processing with SQLite database generation.
This work was supported by the National Cancer Institute and Tufts Integrative 
Cancer Biology Program under U54CA149233-029689. Maintenance is supported by the Cleveland Clinic Foundation. }
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{getFields},\link{pickFields} } }
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
