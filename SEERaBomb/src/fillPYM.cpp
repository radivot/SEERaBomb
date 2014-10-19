#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix fillPYM(SEXP PY_, SEXP PYM_) {
    NumericMatrix PY(PY_) ;
    NumericMatrix PYM(PYM_) ;
    int nrows = PY.nrow(), quo,strtYrIndx,strtAgeIndx;
    double rem, pystrip;
    for (int i = 0; i < nrows ; i++) {
     pystrip=PY(i,0);
     rem = pystrip-floor(pystrip);
     quo = floor(pystrip);
     strtAgeIndx=roundl(PY(i,1)+.5)-1;
     strtYrIndx=roundl(PY(i,2)-1972)-1;
     for (int j = 0; j <= quo ; j++)   
       if (j==quo) 
        PYM(strtAgeIndx+j,strtYrIndx+j)=PYM(strtAgeIndx+j,strtYrIndx+j)+rem;
        else
          PYM(strtAgeIndx+j,strtYrIndx+j)=PYM(strtAgeIndx+j,strtYrIndx+j)+1;
    }  
    return PYM;
}

