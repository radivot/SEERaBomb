\name{nvsr}
\alias{nvsr}
\docType{data}
\title{National Vital Statistic Report (nvsr) Data}
\description{
  US mortality rates (probability of death that year) in 2010; report published Nov. 2014.   
}
\usage{
nvsr
}
\format{
  A data frame with the following columns.
  \describe{
    \item{\code{age}}{Single-year resolution ages up to 99.5. }
    \item{\code{p}}{Pooled sexes and races. }
    \item{\code{pm}}{Pooled races, males. }
    \item{\code{pf}}{Pooled races, females. }
    \item{\code{w}}{Whites, sexes pooled.}
    \item{\code{b}}{Blacks, sexes pooled.}
    \item{\code{o}}{Others, sexes pooled.}
    \item{\code{wm}}{White males. }
    \item{\code{wf}}{White females. }
    \item{\code{bm}}{Black males. }
    \item{\code{bf}}{Black females. }
    \item{\code{om}}{Other males. }
    \item{\code{of}}{Other females. }
  }
}

\examples{
library(SEERaBomb)
head(nvsr)
}  

\references{
National Vital Statistics Reports, Vol. 63, No. 7, November 6, 2014
\url{https://www.cdc.gov/nchs/data/nvsr/nvsr63/nvsr63_07.pdf}
}

\keyword{datasets}
