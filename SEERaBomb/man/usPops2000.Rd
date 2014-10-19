\name{usPops2000}
\alias{us19}
\alias{us86}
\alias{us100}
\docType{data}
\title{The US population in 2000.}
\description{
  Data frames of the US population in 2000, as functions of different resolutions and ranges of age, are provided. 
  The data.frames are: us19 = the 19 age groups of SEER; 
  us86 = the 86 age groups of SEER; and
  us100 = an extension of us86 to 99 years of age.  In the first two the eldest are 
  represented by 90 and in the third 100 is used.   
}
\usage{
us19
us86
us100
}
\format{
  The data frames have the following columns.
  \describe{
    \item{\code{age19, age86, or age100}}{Representative age of group. The number of age groups 
    is 19, 86 or 100, in, respectively, us19, us86 or us100. }
    \item{\code{pop}}{The population within each age group. }
    \item{\code{prop}}{Proportion of the total population within each age group. }
  }
}

\details{Males and females are pooled.  This population data is used to map age specific incidence rate vectors
into summarizing scalars. It allows differences in cancer incidence rates across different
SEER registries to be compared without concerns of differences in age distributions of the populations. 
This statement also holds for differences in age-distributions between sexes; males 
and females are pooled to produce a single reference population to enable such sex comparisons. 
  }

\examples{
library(SEERaBomb)
with(us100,plot(age100,pop/1e6,type="l",xlab="Age",
     ylab="People (Millions)",main="US Population in 2000"))
library(dplyr)
us100\%.\%filter(age100>=85)\%.\%summarize(weighted.mean(age100,w=pop)) 
### so ave age >=85.0 is 89.4 and 90 is indeed representative of >85.0 in us19 and us86

}  

\references{
\url{http://seer.cancer.gov/stdpopulations/}
}


\keyword{datasets}
