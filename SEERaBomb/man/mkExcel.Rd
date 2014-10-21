\name{mkExcel}
\alias{mkExcel}
\title{Makes an excel file that contains relative risks vs. time-since-exposure}
\description{Expected cases (E) under a null hypothesis that prior cancers do not impact subsequent cancer risks 
and observed cases (O) yields relative risks (RR=O/E) and their CI.  Here sheet tabs are first cancers, 
sheet rows are second cancers, and columns are times intervals since first cancer diagnosis.}
\usage{mkExcel(L,fn,outDir="~/Results")} 
\arguments{
  \item{L}{List of lists made by tsx2.  }
  \item{fn}{Excel file name. I recommend using the base file in seerSet produced by mk2D.  }
  \item{outDir}{folder that will hold the output file.}
}

\value{None. The output files are for a given sex and race.
   Second cancer relative risks are given as functions of tsx and different first cancers and first cancer therapies 
   (typically irradiated or not). 
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }
\keyword{IO}
