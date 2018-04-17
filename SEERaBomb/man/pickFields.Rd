\name{pickFields}
\alias{pickFields}
\title{Pick SEER fields of interest}
\description{ Reduces the full set of SEER data fields to a smaller set of interest. SEER fields 
are rows of the input and output dataframes
of this function. The output dataframe differs from the input dataframe not only in there being fewer rows  
but also in there being an additional column needed by \code{mkSEER()} downstream.}
\usage{pickFields(sas,picks=c("casenum","reg","race","sex","agedx",
        "yrbrth","seqnum","modx","yrdx","histo3",%"radiatn", %"recno","agerec",
        "ICD9","COD","surv","radiatn","chemo"))}  % removed "numprims", just before COD 

\arguments{
  \item{sas}{A data frame created by \code{getFields()} using the SAS file found in 
  the \file{incidence} directory of \code{seerHome}, the root of the SEER ASCII data installation.}
  \item{picks}{ Vector of names of variables of interest. This set should not be smaller than the default.   
%The default is \code{picks=c("casenum","reg","race",} 
%\code{"sex","agedx","yrbrth","seqnum","yrdx","histo2","histo3","radiatn",}

%\code{"agerec","ICD9","histrec","numprims","COD","surv")}
  }
}
\details{R binaries become too large if all of the fields are selected. \code{SEERaBomb} is faster than SEER*Stat 
because it tailors/streamlines the database to your interests. The default picks are a reasonable place to start; if you 
determine later that you need more fields, you can always rebuild the binaries. Grabbing all fields is  
discouraged, but if you want this anyway, note that you still need \code{pickFields()} to create a data type column, i.e. you cannot bypass \code{pickFields()} by sending the output of \code{getFields()} straight to \code{mkSEER()}.}

\value{The SAS-based input data frame \code{sas}, shortened to just the rows of \code{picks}, and expanded to include 
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
