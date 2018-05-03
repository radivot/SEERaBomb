\name{incidAbomb}
\alias{incidAbomb}
\title{Computes A-bomb incidences}
\description{Creates A-bomb survivor incidence rates and confidence intervals. }
\usage{incidAbomb(d)} 
\arguments{
  \item{d}{Tibble, typically grouped, with DG column ending 1st block and py starting the last.}
}

\details{The columns  \code{DG} and  \code{py} must exist in  \code{d}, in that order. 
Person-year weighted means will be formed of any columns between them. 
Its OK if none exist. Sums are formed on \code{py} and anything to their right. It is assumed
that cancer types begin after py, with at most upy and/or subjects intervening them. 
}

\value{A tibble data frame, summarized by groups, with cancers after py in a new cancers column,
and new columns O (observed cases), incid and incid 95\% CI limits LL and UL.}

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package} }}
%\examples{
%library(SEERaBomb)
%}
\keyword{IO}
