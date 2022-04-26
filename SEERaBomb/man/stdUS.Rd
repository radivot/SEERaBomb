\name{stdUS}
\alias{stdUS}
\docType{data}
\title{The standard population of the US in 2000}
\description{
  The US population in 2000 for ages up to 100 years. Sexes are pooled.  
}
\usage{
stdUS
}
\format{
  A data frame with the following columns.
  \describe{
    \item{\code{age}}{Sigle-year resolution ages up to 100. }
    \item{\code{pop}}{The population within each age group. }
    \item{\code{prop}}{Proportion of the total population within each age group. }
  }
}

\details{This population data can be used to map age specific incidence rate vectors
into summarizing scalars. It allows cancer incidence rates across different
SEER registries to be compared without concerns of differences in age distributions of the populations. 
  }

\examples{
library(SEERaBomb)
with(stdUS,plot(age,pop/1e6,type="l",xlab="Age",
     ylab="People (Millions)",main="US Population in 2000"))
library(dplyr)
stdUS\%>\%filter(age>=85)\%>\%summarize(weighted.mean(age,w=pop)) 
### so ave age >=85.0 is 89.4
}  

\references{
\url{https://seer.cancer.gov/stdpopulations/}
}


\keyword{datasets}
