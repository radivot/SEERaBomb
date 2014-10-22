\name{seerSet}
\alias{seerSet}
\title{Joins SEER cancers and population person years in a single list}
\description{This function coordinates slicing of cancers and population person years. It also removes
        cancer factor levels no longer present after slicing, and place the results into a list that 
        can be attached or with-ed to immediately specify the specs of the slicing.}
\usage{seerSet(canc,popsa,ageStart=15,ageEnd=85,Sex="male", Race="pool", picks=NULL)} 
\arguments{
  \item{canc}{data.frame of cancers that should include at least the columns/fields age86, sex, race, yrdx and cancer }
  \item{popsa}{data.frame of single-age population person years.  }
  \item{ageStart}{Starting age of fits. Cases and PY greater than or equal to this age are used.}
  \item{ageEnd}{Ending age of fits. Cases and PY strictly less than this age are used.}
  \item{Sex}{Choose either "male" (default) or "female", not both: 2D spline objects are big so only one sex at a time. }
  \item{Race}{Choose either "white", "black", "other",  or "pool" (default).}
  \item{picks}{Keep this at NULL to get all cancers, as is needed for multiple primary work. 
  Use a charcter vector of cancer types to focus on only when trying to improve spline fits with feedback from
  plot2D.}
}

\value{A list containing the subsets of canc and popsa and information regarding how they were reduced.  
  In the outputs, age86 is renamed as age and yrdx is copied into year, to prepare for joinings and plots.
  Within canc, it is critical that users relevel cancer if needed, and 
   define a trt column, before using mk2D.}

\note{It is critical that users define a trt column in canc before sending it into here. If users want to define a cancer
differently from the defaults, that should also be done to canc before entry here.After the data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D}, \link{plot2D}} }
\keyword{IO}
