\name{mkSEERold}
\alias{mkSEERold}
\title{ Make SEER binaries as before  }
\description{ Converts SEER ASCII text files into smaller R binaries.
       This is being maintained to avoid fixing old scripts. Please use mkSEER for new scripts.}
\usage{mkSEERold(df,seerHome="~/data/SEER",
            dataset=c("00","73","92"),SQL=TRUE, mkDFs=FALSE)}

\arguments{
  \item{df}{ A data frame that was the output of \code{pickFields}. This determines which fields to transfer. }
  \item{seerHome}{ The directory that contains the SEER \file{population} and \file{incidence} directories.  This should be writable by the user.}
  \item{dataset}{The SEER database to use, specified as a string of the last two digits of the starting year, i.e. \code{"73"} = 1973-2011, \code{"92"}  = 1992-2011, and \code{"00"} = 2000-2011. This determines the subdirectory in \file{incidence} and \file{population} from which data is taken and is also the name of the subdirectory of \code{seerHome} to which the R data files will be written, e.g. \code{"00"} processes the 2000 database and places the binary results in /data/SEER/00.}
  \item{SQL}{TRUE if an SQLite database is to be created. The file \file{all.db} produced in this case can be significantly larger than the sum of the \file{*.RData} files also produced. }           
  \item{mkDFs}{TRUE if you wish to make data frame binaries. }           
}

\details{See \link{mkSEER}.}
\value{None.  This function produces R binary data files.}

\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER},\link{getFields},\link{pickFields} } }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
(df=pickFields(df))
for (i in c("73","92","00")) mkSEERold(df,dataset=i)
}
}
\keyword{internal}

