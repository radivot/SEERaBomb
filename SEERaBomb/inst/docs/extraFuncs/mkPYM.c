#include <R.h>
#include <Rinternals.h>

SEXP mkPYM(SEXP py, SEXP age, SEXP year, SEXP nyears)
     {
       R_len_t nr;
       int i, j, quo,strtYrIndx,strtAgeIndx,*xnyears;
       double *xpy,*xage,*xyear, *xout, rem, pystrip;
       PROTECT(py = coerceVector(py, REALSXP));
       PROTECT(age = coerceVector(age, REALSXP));
       PROTECT(year = coerceVector(year, REALSXP));
       PROTECT(nyears = coerceVector(nyears, INTSXP));
       SEXP out = PROTECT(allocVector(REALSXP,5040 ));
       nr = length(py);
       xpy = REAL(py);xage = REAL(age);xyear = REAL(year); xnyears = INTEGER(nyears); xout = REAL(out);
/*       memset(xout, 0, 5040 * sizeof(double)); */
       for(i = 0; i < 5040; i++) xout[i] = 0.0;  
    for (i = 0; i < nr ; i++) {
     pystrip=xpy[i];
     rem = pystrip-floor(pystrip);
     quo = floor(pystrip);
     strtAgeIndx=roundl(xage[i]+.5)-1;
     strtYrIndx=roundl(xyear[i]-1972)-1;
     for (j = 0; j <= quo ; j++)  
      if ( ((strtAgeIndx+j)<126) & ((strtYrIndx+j)<*xnyears)) {
        if (j==quo) 
         xout[strtAgeIndx+j+126*(strtYrIndx+j)]+=rem;
        else
         xout[strtAgeIndx+j+126*(strtYrIndx+j)]+=1;
      }
          
    }  
       UNPROTECT(5);
       return(out);
}

