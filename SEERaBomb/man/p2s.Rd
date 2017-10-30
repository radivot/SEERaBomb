\name{p2s}
\alias{p2s}
\title{Primary to Secondary}
\description{Using a SEER data frame, this function computes times between primary and secondary cancers. In the resulting data frame, surv and status can be analyzed at the individual level, e.g. using Cox regression. 
}
\usage{p2s(canc,firstS,secondS,yrcut=2010) } 
\arguments{
  \item{canc}{Data frame produced by mkSEER().  }
  \item{firstS}{Vector of names (as Strings) of first cancers you wish to consider. }
  \item{secondS}{Vector of names of second cancers you wish to consider. }
  \item{yrcut}{Only cases diagnosed in yrcut or newer are analyzed. The default of 2010 is the year AML cases after MDS
  began to be entered into SEER as second cancers; before they were considered to be part of the first cancer.
  This function facilitates studies of the rate at which myeloid neoplasms such as MDS progress to AML. }
}

\value{Data frame with a row for each primary (first cancer) diagnosed on or after yrcut. The surv column holds the
time in months to last follow up or death (status=0), or to the time of diagnosis of the second cancer (status=1). 
     }

\author{Remco J. Molenaar (r.j.molenaar@amc.uva.nl )}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }

\examples{
\dontrun{
# 
}
}

\keyword{IO}
