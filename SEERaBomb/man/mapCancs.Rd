\name{mapCancs}
\alias{mapCancs}
\title{Map SEER ICD9 and ICD-O3 (histo3) codes to cancer names}
\description{ This function adds a column called cancer to a cancer data.frame or tbl.}
\usage{mapCancs(D)} 
\arguments{
  \item{D}{A large cancer dataframe that includes ICD9 and histo3 as columns.  }
}

\value{The input dataframe with an additional cancer column at the end.}

\note{This function is used by mkSEER() when generating merged R binaries. }
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mapRegs}} }
\keyword{IO}
