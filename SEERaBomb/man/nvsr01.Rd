\name{nvsr01}
\alias{nvsr01}
\docType{data}
\title{National Vital Statistic Report (nvsr01) Data}
\description{
  US mortality rates (probability of death that year) in 2001 (Report 52_14)   
}
\usage{
nvsr01
}
\format{
  A data frame with the following columns.
  \describe{
    \item{\code{age}}{Single-year resolution ages up to 99.5. }
    \item{\code{p}}{Pooled sexes and races. }
    \item{\code{pm}}{Pooled races, males. }
    \item{\code{pf}}{Pooled races, females. }
    \item{\code{wm}}{White males. }
    \item{\code{wf}}{White females. }
    \item{\code{bm}}{Black males. }
    \item{\code{bf}}{Black females. }
  }
}

\details{This data is used to extrapolate PY at risk in SEER population files from 85+ to older ages.  
  }

\examples{
library(SEERaBomb)
head(nvsr01)
}  

\references{
\url{ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Publications/NVSR/52_14/}
}

\keyword{datasets}
