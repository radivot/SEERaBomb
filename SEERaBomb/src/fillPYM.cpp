#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix fillPYM(SEXP PYin, SEXP PYM) {
  NumericMatrix PY1(PYin);
  NumericMatrix PYM1(PYM);
  int nrows = PY1.nrow(),nages = PYM1.nrow(),nyears = PYM1.ncol(), quo,strtYrIndx,strtAgeIndx;
  double rem, pystrip, pre,age;
  for (int i = 0; i < nrows ; i++) {
    age=PY1(i,1);
    pre=ceil(age)-age;
    if (pre==0) pre=1;
    pystrip=PY1(i,0);
    if (pystrip>pre) {
      pystrip=pystrip-pre; 
      rem = pystrip-floor(pystrip);
      quo = floor(pystrip);
    }
    else {
      pre=pystrip; 
      pystrip=0;
      rem=0;
      quo=0;
    }
    strtAgeIndx=floor(age);
    strtYrIndx=PY1(i,2)-1973;
    /*    strtAgeIndx=roundl(PY1(i,1)+.5)-1;
    strtYrIndx=roundl(PY1(i,2)-1972)-1; */
    for (int j = 0; j <= (quo+1) ; j++)   {
      if ( (strtAgeIndx+j<nages) & (strtYrIndx+j<nyears)) {
        if (j==0) PYM1(strtAgeIndx+j,strtYrIndx+j)+=pre; else
        if (j==(quo+1)) 
        PYM1(strtAgeIndx+j,strtYrIndx+j)+=rem;
        else
        PYM1(strtAgeIndx+j,strtYrIndx+j)+=1;
      }
    }
  }  
  return PYM1;
}

