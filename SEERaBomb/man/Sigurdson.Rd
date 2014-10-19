\name{Sigurdson}
\alias{Sigurdson}
\docType{data}
\title{Chromosome translocation versus age data}
\description{
  This is chromosome translocation versus age data that is pooled across gender and race.    
}
\usage{data(Sigurdson)}
\format{
  A data frame named Sigurdson with the following columns.
  \describe{
    \item{\code{age}}{Age of donor of lymphocytes. }
    \item{\code{tlcn}}{Total number of chromosomal translocations per 100 cell equivalents. }
  }
}

\details{The data were obtained using FISH, see reference below. This dataset is loaded automatically with 
\code{library(SEERaBomb)}. As such, the function \code{data()} is not needed.  }

\examples{
library(SEERaBomb)
with(Sigurdson,plot(age,tlcn,cex=2,cex.axis=2,cex.lab=2,las=1,cex.main=2,
               ylab="",main="Translocations per 100 cells"))
}  

\references{
Sigurdson et al. Mutation Research 652 (2008) 112-121
}


\keyword{datasets}
