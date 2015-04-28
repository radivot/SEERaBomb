\name{mkExcel}
\alias{mkExcel}
\title{Makes an excel file that contains relative risks vs. time-since-diagnosis}
\description{Expected cases (E) under a null hypothesis that prior cancers do not impact subsequent cancer risks 
and observed cases (O) yields relative risks (RR=O/E) and their CI.  Here sheet tabs are first cancers, 
sheet rows are second cancers, and columns are times intervals since first cancer diagnosis.}
\usage{mkExcel(seerSet,tsdn,outDir="~/Results", txt=NULL,flip=FALSE)} 
\arguments{
  \item{seerSet}{As made by tsx2.  }
  \item{tsdn}{Time since diagnosis set name. The seerSet base file name (bfn) + tsdn + txt will be the excel file name.}
  \item{outDir}{folder that will hold the output file.}
  \item{txt}{If needed, additional text in file name to make it unique, e.g. for a modification in a
               cancer or treatment definition. }
  \item{flip}{FALSE: sheets are first cancers, rows seconds. TRUE: sheets are second cancers, rows firsts.}

}

\value{The output files are for a given sex and race.
   Relative risks by second cancers (as rows) are given as functions of tsd 
   and different first cancers (sheets) and first cancer therapies 
   (typically irradiated or not). A corresponding list of dataframes is returned invisibly. 
     }

\note{Examples are not possible without data. After the SEER data is installed, see the script mkRRtsx.R in the examples folder. This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}
\seealso{\code{\link{SEERaBomb-package}, \link{mk2D},\link{seerSet}} }
\keyword{IO}
