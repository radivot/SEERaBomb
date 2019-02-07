\name{mkMrtLocal}
\alias{mkMrtLocal}
\title{Make mortality binaries from local HMD data files}
\description{Converts locally installed Human Mortality Data \url{http://www.mortality.org/} into an R binary file
 \file{mrtCOUNTRY.RData}.}
\usage{mkMrtLocal(country="USA",mrtHome="~/data/mrt",
                    mrtSrc1="~/data/hmd_countries",
                    mrtSrc2="~/data/hmd_statistics/death_rates/Mx_1x1"
                    )}

\arguments{
  \item{country}{Default is USA. See names of subfolders of \file{hmd_countries} for other options.}
  \item{mrtHome}{Directory that will contain the mortality data binary.  Should be writable by user.}
  \item{mrtSrc1}{Directory with hmd_countries data (first choice of files = "all HMD countries").}
  \item{mrtSrc2}{Directory with hmd_statistics data (second choice of files = "all HMD statistics").}
}

\value{None. This function is called for its side-effect of producing \file{mrt.RData} from HMD files
      organized as all HMD countries or all HMD statistics on the HMD download page (you need at least one of these). }
\references{ Barbieri M, Wilmoth JR, Shkolnikov VM, et al. Data Resource Profile: 
             The Human Mortality Database (HMD). Int J Epidemiol. 2015;44: 1549-1556.}
\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkSEER}} }
\examples{
\dontrun{
library(SEERaBomb)
mkMrtLocal() 
load("~/data/mrt/mrtUSA.RData")
head(mrt$Female)
}
}
\keyword{IO}
