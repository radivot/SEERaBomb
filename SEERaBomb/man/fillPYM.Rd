\name{fillPYM}
\alias{fillPYM}
%\docType{package} %dirk has this in his function help page

\title{Fills age-year person year (PY) matrix}
\description{This function converts a matrix with lots of individual PY contributions and starting ages and years as rows, 
into a population-level PY histrogram matrix of (1-year age)x(1-year year) resolution bins. The output matrix is
more square-like than the tall dataframe-like input matrix, currently at 39 calendar years by 122 age years.
}
\usage{
fillPYM(PY,PYM)
}

\arguments{
  \item{PY}{Tall input matrix of individual survival time person years at risk.  }
  \item{PYM}{PY histogram or coverage matrix with age and calendar years both a single year resolution.  }
}

\value{The second argument is the output. The matrix should be filled with zeros in R before calling this 
function. After the call, it will be filled. It works like this because it is implemented using pointers in Rcpp C++ code. }

\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}


\examples{
(mats=matrix(0,ncol=20,nrow=70))
(PYtest=structure(c(3.5, 11.25, 51.5, 58.5, 1974, 1976, 55, 69.75),.Dim = c(2L,4L), 
                  .Dimnames=list(c("1","10"),c("py", "ageL", "year", "ageR"))))
fillPYM(PYtest, mats)
\dontrun{
}
}

