\name{incidSEER}
\alias{incidSEER}
\title{Computes SEER incidences}
\description{Creates SEER incidence rates and confidence intervals. }
\usage{incidSEER(canc,popsae,cancers)} 
\arguments{
  \item{canc}{data frame of cancer cases}
  \item{popsae}{data frame of person years at risk}
  \item{cancers}{character vector of cancer types}
}

\details{This left joins popsae and cancers in canc.}

\value{A data frame with observed cases (O), incid, and incid 95\% CI limits LL and UL.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package} }}
%\examples{
%library(SEERaBomb)
%}
\keyword{IO}
