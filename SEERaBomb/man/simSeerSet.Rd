\name{simSeerSet}
\alias{simSeerSet}
\title{Simulate SEER cancers and population person years}
\description{Simulated data, with known underlying distributions and correlations,
             can be used to test downstream software.}
\usage{simSeerSet(N=2e9,yearEnd=2012,ka=1e-6,kb=0.04,Ab=1e-6,Rba=1e-3)} 
\arguments{
  \item{N}{Number of person years to simulate. Default is roughly that of SEER. },
  \item{yearEnd}{Most recent SEER year to simulate. },
  \item{ka}{The rate at which incidence increases linearly with age for cancer A. },
  \item{kb}{The exponential aging rate constant for cancer B incidence. },
  \item{Ab}{The exponential amplitude for cancer B incidence. },
  \item{Rba}{The risk of B given A. This constant is assumed to be independent of age and year. 
  Latencies are assumed to follow a beta distribution between 0 and 5 years with a peak at just under 4 years.}
}

\value{A simulated seerSet object with popsa filled using US 2000 Std population proportions and canc with 
cancers A and B
where the incidence of A increases linearly with age and B increase exponentially in age.
Survival times are assumed to be exponentially distributed with means of 5 years for A and 2 years for B.}

\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{seerSet},\link{mk2D}, \link{plot2D}} }
\keyword{IO}
