/* fishPBPK.c for R deSolve package
   ___________________________________________________

   Model File:  fishPBPK.model

   Date:  Thu Jun 20 16:17:17 2019

   Created by:  "C:/MCSIM-~1.6/mod/.libs/mod.exe v5.6.6"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2017 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   8 States:
     cfat = 0.0,
     cliv = 0.0,
     cspf = 0.0,
     crpf = 0.0,
     ckdn = 0.0,
     cmet = 0.0,
     ains = 0.0,
     insswch = 0.0,

   3 Outputs:
    "cv",
    "ca",
    "mbal",

   0 Inputs:

   35 Parameters:
     bw = 0,
     qc = 0,
     qg = 0,
     vfatc = 0,
     qfatc = 0,
     pfat = 0,
     vlivc = 0,
     qlivc = 0,
     pliv = 0,
     vkdnc = 0,
     qkdnc = 0,
     pkdn = 0,
     vrpfc = 0,
     qrpfc = 0,
     prpf = 0,
     vspfc = 0,
     qspfc = 0,
     pspf = 0,
     frspfkdn = 0.4,
     vfat = 0,
     vkdn = 0,
     vliv = 0,
     vrpf = 0,
     vspf = 0,
     qfat = 0,
     qkdn = 0,
     qliv = 0,
     qrpf = 0,
     qspf = 0,
     vmax = 0,
     km = 1e-10,
     cins = 0,
     pbldw = 1e10,
     gul = 1,
     intcl = -1,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_cfat 0x00000
#define ID_cliv 0x00001
#define ID_cspf 0x00002
#define ID_crpf 0x00003
#define ID_ckdn 0x00004
#define ID_cmet 0x00005
#define ID_ains 0x00006
#define ID_insswch 0x00007

/* Model variables: Outputs */
#define ID_cv 0x00000
#define ID_ca 0x00001
#define ID_mbal 0x00002

/* Parameters */
static double parms[35];

#define bw parms[0]
#define qc parms[1]
#define qg parms[2]
#define vfatc parms[3]
#define qfatc parms[4]
#define pfat parms[5]
#define vlivc parms[6]
#define qlivc parms[7]
#define pliv parms[8]
#define vkdnc parms[9]
#define qkdnc parms[10]
#define pkdn parms[11]
#define vrpfc parms[12]
#define qrpfc parms[13]
#define prpf parms[14]
#define vspfc parms[15]
#define qspfc parms[16]
#define pspf parms[17]
#define frspfkdn parms[18]
#define vfat parms[19]
#define vkdn parms[20]
#define vliv parms[21]
#define vrpf parms[22]
#define vspf parms[23]
#define qfat parms[24]
#define qkdn parms[25]
#define qliv parms[26]
#define qrpf parms[27]
#define qspf parms[28]
#define vmax parms[29]
#define km parms[30]
#define cins parms[31]
#define pbldw parms[32]
#define gul parms[33]
#define intcl parms[34]

/* Forcing (Input) functions */
static double forc[1];


/* Function definitions for delay differential equations */

int NoutfishPBPK=1;
int nrfishPBPK[1]={0};
double ytaufishPBPK[1] = {0.0};

static double yini[8] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

void lagvaluefishPBPK(double T, int *nrfishPBPK, int N, double *ytaufishPBPK) {
  static void(*fun)(double, int*, int, double*) = NULL;
  if (fun == NULL)
    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvaluefishPBPK");
  return;
}

double CalcDelayfishPBPK(int hvar, double dTime, double delay) {
  double T = dTime-delay;
  if (dTime > delay){
    nrfishPBPK[0] = hvar;
    lagvaluefishPBPK( T, nrfishPBPK, NoutfishPBPK, ytaufishPBPK );
}
  else{
    ytaufishPBPK[0] = yini[hvar];
}
  return(ytaufishPBPK[0]);
}

/*----- Initializers */
void initmodfishPBPK (void (* odeparms)(int *, double *))
{
  int N=34;
  odeparms(&N, parms);
}

void initforcfishPBPK (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
}


/* Calling R code will ensure that input y has same
   dimension as yini */
void initStatefishPBPK (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
  {
    yini[i] = y[i];
  }
}

void getParmsfishPBPK (double *inParms, double *out, int *nout) {
/*----- Model scaling */

  int i;

  for (i = 0; i < *nout; i++) {
    parms[i] = inParms[i];
  }


  for (i = 0; i < *nout; i++) {
    out[i] = parms[i];
  }
  }
/*----- Dynamics section */

void derivsfishPBPK (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double rcmet;
  /* local */ double totdose;
  /* local */ double totbody;
  /* local */ double totclear;
  /* local */ double tmass;

  ydot[ID_insswch] = 0 ;

  ydot[ID_ains] = y[ID_insswch] * gul * cins ;

  yout[ID_cv] = ( qfat * y[ID_cfat] / pfat + ( 1 - frspfkdn ) * qspf * y[ID_cspf] / pspf + ( qkdn + frspfkdn * qspf ) * y[ID_ckdn] / pkdn + ( qliv + qrpf ) * y[ID_cliv] / pliv ) / qc ;

  yout[ID_ca] = yout[ID_cv] + ( y[ID_insswch] * gul * cins - gul * yout[ID_cv] / pbldw ) / qc ;

  ydot[ID_cfat] = qfat * ( yout[ID_ca] - y[ID_cfat] / pfat ) / vfat ;

  ydot[ID_cspf] = qspf * ( yout[ID_ca] - y[ID_cspf] / pspf ) / vspf ;

  ydot[ID_crpf] = qrpf * ( yout[ID_ca] - y[ID_crpf] / prpf ) / vrpf ;

  ydot[ID_ckdn] = ( qkdn * yout[ID_ca] + frspfkdn * qspf * y[ID_cspf] / pspf ) / vkdn - ( qkdn + frspfkdn * qspf ) / vkdn * y[ID_ckdn] / pkdn ;
  
  if(intcl == -1){
  rcmet = ( vmax * y[ID_cliv] / pliv ) / ( km + y[ID_cliv] / pliv ) / vliv ;
  }
  else{
  rcmet = intcl / vliv ;
  }

  ydot[ID_cmet] = rcmet ;

  ydot[ID_cliv] = ( qliv * yout[ID_ca] + qrpf * y[ID_crpf] / prpf ) / vliv - ( qliv + qrpf ) / vliv * y[ID_cliv] / pliv - rcmet ;

  totdose = y[ID_ains] ;

  totbody = y[ID_cfat] * vfat + y[ID_cspf] * vspf + vrpf * vrpf + y[ID_ckdn] * vkdn + y[ID_cliv] * vliv ;

  totclear = y[ID_cmet] * vliv ;

  tmass = totdose - totbody - totclear ;

  yout[ID_mbal] = tmass ;
  yout[ID_ca] = yout[ID_ca] ;
  yout[ID_cv] = yout[ID_cv] ;

} /* derivs */


/*----- Jacobian calculations: */
void jacfishPBPK (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void eventfishPBPK (int *n, double *t, double *y)
{

  y[ID_insswch] = ( cins > 0 ? ( y[ID_insswch] == 0 ? 1 : 0 ) : 0 ) ;

} /* event */

/*----- Roots calculations: */
void rootfishPBPK (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

