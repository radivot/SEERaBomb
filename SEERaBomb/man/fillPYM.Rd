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
  \item{PYM}{PY histogram or coverage matrix with age and calendar years both at single year resolution.  }
}

\value{The second argument is the output. The matrix should be filled with zeros in R before calling this 
function. After the call, it will be filled. It works like this because it is implemented using pointers in Rcpp C++ code. }

\note{This function was developed with support from the Cleveland Clinic Foundation.}
\author{Tom Radivoyevitch (radivot@ccf.org)}


\examples{
  yrs=1973:1990
  ages=0.5:70.5
  mat=matrix(0,ncol=length(yrs),nrow=length(ages))
  colnames(mat)=yrs
  rownames(mat)=ages
(PYin=structure(c(3.5, 11.25,5.2, 51.5, 58.5,0.75, 1974, 1976,1978),.Dim = c(3L,3L), 
                  .Dimnames=list(c("1","2","3"),c("py", "ageL", "year"))))
fillPYM(PYin, mat)

\dontrun{
}
}

