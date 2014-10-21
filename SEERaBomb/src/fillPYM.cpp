#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix fillPYM(SEXP PY, SEXP PYM) {
    NumericMatrix PY1(PY) ;
    NumericMatrix PYM1(PYM) ;
    int nrows = PY1.nrow(), quo,strtYrIndx,strtAgeIndx;
    double rem, pystrip;
    for (int i = 0; i < nrows ; i++) {
     pystrip=PY1(i,0);
     rem = pystrip-floor(pystrip);
     quo = floor(pystrip);
     strtAgeIndx=roundl(PY1(i,1)+.5)-1;
     strtYrIndx=roundl(PY1(i,2)-1972)-1;
     for (int j = 0; j <= quo ; j++)   
       if (j==quo) 
        PYM1(strtAgeIndx+j,strtYrIndx+j)=PYM1(strtAgeIndx+j,strtYrIndx+j)+rem;
        else
          PYM1(strtAgeIndx+j,strtYrIndx+j)=PYM1(strtAgeIndx+j,strtYrIndx+j)+1;
    }  
    return PYM1;
}

