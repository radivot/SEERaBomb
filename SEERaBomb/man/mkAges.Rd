\name{mkAges}
\alias{mkAges}
\title{ Make File Containing Mean Ages of 5-year Age Groups across Years and Sexes}
\description{Uses Human Mortality Data (HMD) to produce mean ages of 5-year age groups.}
\usage{mkAges(mrtHome="~/data/mrt",country="USA")}

\arguments{
  \item{mrtHome}{The directory that contains the mrtCOUNTRY.RData file created by SEERaBomb::mkMrtLocal().}
  \item{country}{HMD country of interest. }
}

\details{Defaults assume "~/data/mrt/mrtUSA.RData" was already made by SEERaBomb::mkMrtLocal()}
\value{None, it produces a file of mean ages, e.g. "~/data/mrt/agesUSA.RData".}

%\note{.}
\author{ Tom Radivoyevitch (\email{radivot@gmail.com}) }
\seealso{\code{\link{SEERaBomb-package},\link{mkCML} } }
\examples{
\dontrun{
library(SEERaBomb)
mkSEER(seerHome="~/data/seer25")  #for SEER data released in the Spring of 2025
}
}
\keyword{IO}
