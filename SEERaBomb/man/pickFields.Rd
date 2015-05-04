\name{pickFields}
\alias{pickFields}
\title{Pick SEER fields of interest}
\description{ Reduces the full set of SEER data fields to a smaller set of interest.}
\usage{pickFields(sas,picks=c("casenum","reg","race","sex","agedx",
        "yrbrth","seqnum","modx","yrdx","histo3","radiatn","recno",
        "agerec","ICD9","numprims","COD","surv"))} 

\arguments{
  \item{sas}{ A data frame created by \code{getFields()} using the SAS file found in 
  the \file{incidence} directory of \code{seerHome}, the root of the SEER ASCII data installation.}
  \item{picks}{ Vector of names of variables of interest. These must be in the same order as found in the 
  input data frame \code{sas}, and must at least include the bare minimum of all of the defaults given here.
%The default is \code{picks=c("casenum","reg","race",} 
%\code{"sex","agedx","yrbrth","seqnum","yrdx","histo2","histo3","radiatn",}
%\code{"agerec","ICD9","histrec","numprims","COD","surv")}
  }
}
\details{R binaries become too large if all of the fields are selected. \code{SEERaBomb} is faster than SEER*Stat 
because it tailors/streamlines the database to your interests.}

\value{The SAS-based input data frame \code{sas}, shortened to just the rows of \code{picks}, and then expanded to include 
spacer rows of fields of no interest pooled into single strings: the width of such a spacer row is equal to 
the distance in bytes between the fields of interest above and below it. This data frame is then 
used by \code{laf_open_fwf()} of \pkg{LaF} in \code{mkSEER()} to read the SEER files.  Proper use of this function, and of the SEER data in general,
requires an understanding of the contents of \file{seerdic.pdf} in the \file{incidence} directory of \code{seerHome}.
}


\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{getFields}, \link{pickFields}, \link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
(df=getFields())
(df=pickFields(df))

}
}
\keyword{IO}
