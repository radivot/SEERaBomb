\name{fillPYM}
\alias{fillPYM}
\alias{SEERaBomb_fillPYM}

\title{Fills age-year person year (PY) matrix}
\description{This internal function converts a matrix with lots of individual PY contributions and starting ages and years as rows, 
into a population-level PY histrogram matrix of (1-year age)x(1-year year) resolution bins. The output matrix is
more square-like (currently at 40 calendar years by 126 age years) than the tall dataframe-like input matrix.
}
\usage{
fillPYM(PYin,PYM)
%SEERaBomb_fillPYM(PYin,PYM)
}

\arguments{
  \item{PYin}{Tall input matrix where rows hold individual PY at risk, year and starting (left) ages ageL.  }
  \item{PYM}{Output PY matrix with age and calendar year rows and columns at single year resolution.  }
}

\value{The second argument becomes the output. This matrix should be filled with zeros in R before calling this 
function. After the call, it will be filled; this function uses pointers in Rcpp C++ code. }

\author{Tom Radivoyevitch (radivot@ccf.org)}


\examples{
  yrs=1975:1990
  ages=0.5:70.5
  PYM=matrix(0,ncol=length(yrs),nrow=length(ages))
  colnames(PYM)=yrs
  rownames(PYM)=ages
(PYin=structure(c(3.5, 11.25,5.2, 51.5, 58.5,0.75, 1976, 1977,1980),.Dim = c(3L,3L), 
                  .Dimnames=list(c("1","2","3"),c("py", "ageL", "year"))))
fillPYM(PYin, PYM)
}
\keyword{internal}

