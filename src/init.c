#include <R.h>
#include <Rinternals.h>
         #include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void getParms(double *, double *, void *);

extern void initmod(void *);
;
extern void derivs(int *, double *, double *, double *, double *, int *);

extern void jac(int *, double *, double *, int *, int *, double *, int *, double *, int *);

extern void event(int *, double *, double *);

extern void root (int *, double *, double *, int *, double *, double *, int *);

static const R_CMethodDef CEntries[] = {
    {"getParms",       (DL_FUNC) &getParms,       3},
  
    {"initmod", (DL_FUNC) &initmod, 1},
  
    {"derivs", (DL_FUNC) &derivs, 6},
   
    {"jac", (DL_FUNC) &jac, 9},
  
    {"event",       (DL_FUNC) &event,       3},
   
    {"root",       (DL_FUNC) &root,       7},
  
    {NULL, NULL, 0}
};

void R_init_plethem(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
    R_useDynamicSymbols(dll, TRUE);
}
