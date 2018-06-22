\name{simSurv}
\alias{simSurv}
\title{Simulate Survival Times}
\description{Uses background mortality rates to simulate background survival time for matching sex, age and year.}
\usage{ simSurv(d,mrt,rep=1,ltb=NULL)}

\arguments{
  \item{d}{Data frame containing sex, agedx, yrdx, surv, and status columns of observed data. }
  \item{mrt}{Mortality data binary made by mkMrt().  }
  \item{rep}{Number of simulated replicates of each observed case.  }
  \item{ltb}{Life table data binary made by mkLT(). Default of NULL => skip it.    }
}

\value{Input d with simulation rows added below it, identified as "simulated" in a new column called type.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkLT()
load("~/data/usMort/ltb.RData")
tail(ltb$Female)
}
}
\keyword{IO}
