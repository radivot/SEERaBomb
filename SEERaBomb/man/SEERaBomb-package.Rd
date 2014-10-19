\name{SEERaBomb-package}
\alias{SEERaBomb-package}
\docType{package}
\title{ SEER data setup and examples including analyses of A-Bomb Data }
\description{
This package strives to make SEER data more accessible from R. 
It provides functions that can be combined to convert fixed-width format
SEER ASCII text data files into R binaries. It also provides examples of the use of these binaries and examples that use the Japanese A-bomb survivor data. 
This package works only with the latest SEER release, now of April 15, 2014.
SEER data field positions and names can change over the years, and a goal of 
SEERaBomb is to buffer/protect R scripts from such changes.
}

\details{
\tabular{ll}{
Package: \tab \pkg{SEERaBomb}\cr
Type: \tab Package\cr
Depends: \tab LaF, RSQLite\cr
Suggests: \tab bbmle \cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
LazyData: \tab yes\cr
URL: \tab \url{http://epbi-radivot.cwru.edu/SEERaBomb/SEERaBomb.html}\cr
}

\if{html}{\figure{SEERaBombFig.png}}
\if{latex}{\figure{SEERaBombFig.png}{options: width=5in}}


The most recent addition to this package is the function \code{mkSEER2}. This function will eventually replace \code{mkSEER}, as greater amounts of RAM (>=16 GB) become more common. It makes an SQLite database with all of the cancers from all of the registries merged into one table.  Using \code{mkSEER2} bypasses making intermediates of 9 cancer files *.RData in each of 3 registry folders. If only this large file is needed, \code{mkSEER2} should be used rather than \code{mkSEER} followed by 
\code{pool}. 

\if{html}{\figure{mkSEER2.png}}
\if{latex}{\figure{mkSEER2.png}{options: width=5in}}


}


\note{ This package was created with support from the National Cancer Institute and Tufts Integrative 
        Cancer Biology Program under U54CA149233-029689. 
        Its maintenance is being supported by the Cleveland Clinic Foundation.}

\author{ Tom Radivoyevitch (\email{radivot@ccf.org}) }
\seealso{\code{\link{getFields},\link{pickFields},\link{mkSEER} }}

\keyword{package}
