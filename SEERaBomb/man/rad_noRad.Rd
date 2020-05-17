\name{rad_noRad}
\alias{rad_noRad}
\title{Collapse trt to only rad and noRad}
\description{In csd() output DF,this collapses trt to just rad and noRad and removes rad unknowns.    }
\usage{rad_noRad(DF) } 
\arguments{
  \item{DF}{DF in list output of csd() with default trt field levels 
  "rad.chemo", "rad.noChemo", "noRad.chemo", "noRad.noChemo", "unk.chemo", and "unk.noChemo".} 
}
\value{DF with trt reduced to rad and noRad, removing rows with rad unknown. }
\note{ Filtering unknowns before csd() to save time is not recommended since second 
      cancer treatments do not matter and can thus be unknown. After csd() trt is only the 
      1st cancer trt. }

\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{csd} } }

\examples{ # nothing for now
}

\keyword{IO}
