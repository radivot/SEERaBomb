\name{mapCancs}
\alias{mapCancs}
\title{Map SEER ICD9, histo3 and histo2 codes to cancer names}
\description{ This function adds a column called cancer to a cancer data.frame or tbl.}
\usage{mapCancs(D)} 
\arguments{
  \item{D}{A large cancer dataframe that includes ICD9, histo2 and histo3 as columns.  }
}

\value{The input dataframe with an additional cancer column at the end.}

\note{This function is used internally when generating merged R binaries and is not intended for general use. It was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mkSEER2}} }
\keyword{IO}
