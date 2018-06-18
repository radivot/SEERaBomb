\name{SEERaBomb-package}
\alias{SEERaBomb-package}
\docType{package}
\title{ SEER and A-Bomb Data Analysis Tools }
\description{Creates SEER (Surveillance, Epidemiology and End Results) and Japanese A-bomb survivor data binaries 
             from ASCII sources and provides tools for estimating SEER second cancer risks.     
Depicted below, \code{mkSEER}
merges cancers and populations of all three of the SEER databases into single cancer and 
population data frames.  

%\if{html}{\figure{mkSEERfig2.png}{options: width=420, height=340}}
\if{html}{\figure{mkSEERfig.png}{options: width=420}}
\if{latex}{\figure{mkSEERfig.png}{options: width=5in}}

SEER data field positions and names change over the years and the original purpose of  
SEERaBomb was to buffer/protect R scripts from such changes. A second purpose was to speed up SEER data computations by 
reducing the data [via \code{pickFields()}] to only fields of interest. SEERaBomb now has an additional purpose:
estimating relative risks of SEER second cancers after diagnoses of first cancers, using all three SEER databases.
Note: SEER no longer includes radiation therapy data by default. Users must thus obtain custom SEER treatment data  
\url{https://seer.cancer.gov/data/treatment.html}.
}

\details{
\tabular{ll}{
Package: \tab \pkg{SEERaBomb}\cr
Type: \tab Package\cr
Depends: \tab dplyr,  ggplot2, rgl,   demography\cr
Suggests: \tab bbmle \cr
License: \tab GPL-2\cr
LazyData: \tab yes\cr
URL: \tab \url{http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html}\cr
}
}



\references{    Surveillance, Epidemiology, and End Results (SEER) Program
     (www.seer.cancer.gov) Research Data (1973-2015), National Cancer
     Institute, DCCPS, Surveillance Research Program, Surveillance Systems Branch,
     released April 2018, based on the November 2017 submission.}

\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{getFields},\link{pickFields},\link{mkSEER},\link{mkSEERold},\link{mkAbomb} }}


\keyword{package}
