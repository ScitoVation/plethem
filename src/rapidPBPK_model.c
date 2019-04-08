/* rapidPBPK_2oral.model.c for R deSolve package
   ___________________________________________________

   Model File:  rapidPBPK_2oral.model

   Date:  Wed Nov 14 13:40:49 2018

   Created by:  "C:/MCSIM-~1.6/mod/.libs/mod.exe v5.6.6"
    -- a model preprocessor by Don Maszle
   ___________________________________________________

   Copyright (c) 1993-2017 Free Software Foundation, Inc.

   Model calculations for compartmental model:

   48 States:
     inhswch = 0.0,
     ainh = 0.0,
     aexh = 0.0,
     totodose = 0.0,
     odose = 0.0,
     odose1 = 0.0,
     odose2 = 0.0,
     totddose = 0.0,
     ddose = 0.0,
     ddose1 = 0.0,
     ddose2 = 0.0,
     aLAS = 0.0,
     akent = 0.0,
     afec = 0.0,
     aabsgut = 0.0,
     ivswch = 0.0,
     aiv = 0.0,
     abld = 0.0,
     abfat = 0.0,
     atfat = 0.0,
     abskin = 0.0,
     atskin = 0.0,
     abmusc = 0.0,
     atmusc = 0.0,
     abbone = 0.0,
     atbone = 0.0,
     abbrn = 0.0,
     atbrn = 0.0,
     ablng = 0.0,
     atlng = 0.0,
     abhrt = 0.0,
     athrt = 0.0,
     abgi = 0.0,
     atgi = 0.0,
     abliv = 0.0,
     atliv = 0.0,
     abkdn = 0.0,
     atkdn = 0.0,
     abrpf = 0.0,
     atrpf = 0.0,
     abspf = 0.0,
     atspf = 0.0,
     ametliv1 = 0.0,
     ametliv2 = 0.0,
     aclbld = 0.0,
     auexc = 0.0,
     anabsgut = 0.0,
     aucpls = 0.0,

   53 Outputs:
    "abone",
    "abrn",
    "alng",
    "ahrt",
    "akdn",
    "agi",
    "aliv",
    "arpf",
    "aspf",
    "afat",
    "askin",
    "amusc",
    "cpls",
    "cv",
    "cfat_um",
    "ctfat",
    "cbfat",
    "cskin_um",
    "ctskin",
    "cbskin",
    "cmusc_um",
    "ctmusc",
    "cbmusc",
    "cbone_um",
    "ctbone",
    "cbbone",
    "cbrn_um",
    "ctbrn",
    "cbbrn",
    "clng_um",
    "ctlng",
    "cblng",
    "chrt_um",
    "cthrt",
    "cbhrt",
    "ckdn_um",
    "ctkdn",
    "cbkdn",
    "cgi_um",
    "ctgi",
    "cbgi",
    "cliv_um",
    "ctliv",
    "cbliv",
    "crpf_um",
    "ctrpf",
    "cbrpf",
    "cspf_um",
    "ctspf",
    "cbspf",
    "auc",
    "InstInhDose",
    "mbal",

   0 Inputs:

   129 Parameters:
     mw = 0,
     bdose = 0,
     blen = 0,
     breps = 0,
     totbreps = 0,
     drdose = 0,
     vdw = 0,
     dreps = 0,
     inhdose = 0,
     inhtlen = 0,
     inhdays = 0,
     ivdose = 0,
     ivlen = 0,
     bw = 0,
     qcc = 0,
     hct = 0,
     vbldc = 0,
     perfc = 0,
     kbld = 0,
     respr = 0,
     tv = 0,
     ds = 0,
     uflw = 0,
     gfr = 0,
     frwsol = 0,
     fatvtbc = 0,
     vfatc = 0,
     qfatc = 0,
     pfat = 0,
     skinvtbc = 0,
     vskinc = 0,
     qskinc = 0,
     pskin = 0,
     muscvtbc = 0,
     vmuscc = 0,
     qmuscc = 0,
     pmusc = 0,
     bonevtbc = 0,
     vbonec = 0,
     qbonec = 0,
     pbone = 0,
     brnvtbc = 0,
     vbrnc = 0,
     qbrnc = 0,
     pbrn = 0,
     lngvtbc = 0,
     vlngc = 0,
     qlngc = 0,
     plng = 0,
     hrtvtbc = 0,
     vhrtc = 0,
     qhrtc = 0,
     phrt = 0,
     givtbc = 0,
     vgic = 0,
     qgic = 0,
     pgi = 0,
     fa = 0,
     ka = 0,
     livvtbc = 0,
     vlivc = 0,
     qalivc = 0,
     qvlivc = 0,
     pliv = 0,
     kdnvtbc = 0,
     vkdnc = 0,
     qkdnc = 0,
     pkdn = 0,
     rpfvtbc = 0,
     vrpfc = 0,
     qrpfc = 0,
     prpf = 0,
     spfvtbc = 0,
     vspfc = 0,
     qspfc = 0,
     pspf = 0,
     res = 0,
     fupls = 0,
     veh_flag = 0,
     kfec = 0,
     kVtoL = 0,
     kent = 0,
     vbld = 0,
     vpls = 0,
     vfat = 0,
     vskin = 0,
     vmusc = 0,
     vbone = 0,
     vbrn = 0,
     vlng = 0,
     vhrt = 0,
     vkdn = 0,
     vgi = 0,
     vliv = 0,
     vrpf = 0,
     vspf = 0,
     total_perf = 0,
     qcp = 0,
     qfat = 0,
     qskin = 0,
     qmusc = 0,
     qbone = 0,
     qbrn = 0,
     qlng = 0,
     qhrt = 0,
     qkdn = 0,
     qvliv = 0,
     qgi = 0,
     qaliv = 0,
     qrpf = 0,
     qspf = 0,
     pafat = 0,
     paskin = 0,
     pamusc = 0,
     pabone = 0,
     pabrn = 0,
     palng = 0,
     pahrt = 0,
     pakdn = 0,
     pagi = 0,
     paliv = 0,
     parpf = 0,
     paspf = 0,
     vkm1 = 0,
     vmaxliv = 0,
     km = 0,
     cinh = 0,
     qalv = 0,
     pair = 1e10,
*/

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Rdynload.h>

/* Model variables: States */
#define ID_inhswch 0x00000
#define ID_ainh 0x00001
#define ID_aexh 0x00002
#define ID_totodose 0x00003
#define ID_odose 0x00004
#define ID_odose1 0x00005
#define ID_odose2 0x00006
#define ID_totddose 0x00007
#define ID_ddose 0x00008
#define ID_ddose1 0x00009
#define ID_ddose2 0x0000a
#define ID_aLAS 0x0000b
#define ID_akent 0x0000c
#define ID_afec 0x0000d
#define ID_aabsgut 0x0000e
#define ID_ivswch 0x0000f
#define ID_aiv 0x00010
#define ID_abld 0x00011
#define ID_abfat 0x00012
#define ID_atfat 0x00013
#define ID_abskin 0x00014
#define ID_atskin 0x00015
#define ID_abmusc 0x00016
#define ID_atmusc 0x00017
#define ID_abbone 0x00018
#define ID_atbone 0x00019
#define ID_abbrn 0x0001a
#define ID_atbrn 0x0001b
#define ID_ablng 0x0001c
#define ID_atlng 0x0001d
#define ID_abhrt 0x0001e
#define ID_athrt 0x0001f
#define ID_abgi 0x00020
#define ID_atgi 0x00021
#define ID_abliv 0x00022
#define ID_atliv 0x00023
#define ID_abkdn 0x00024
#define ID_atkdn 0x00025
#define ID_abrpf 0x00026
#define ID_atrpf 0x00027
#define ID_abspf 0x00028
#define ID_atspf 0x00029
#define ID_ametliv1 0x0002a
#define ID_ametliv2 0x0002b
#define ID_aclbld 0x0002c
#define ID_auexc 0x0002d
#define ID_anabsgut 0x0002e
#define ID_aucpls 0x0002f

/* Model variables: Outputs */
#define ID_abone 0x00000
#define ID_abrn 0x00001
#define ID_alng 0x00002
#define ID_ahrt 0x00003
#define ID_akdn 0x00004
#define ID_agi 0x00005
#define ID_aliv 0x00006
#define ID_arpf 0x00007
#define ID_aspf 0x00008
#define ID_afat 0x00009
#define ID_askin 0x0000a
#define ID_amusc 0x0000b
#define ID_cpls 0x0000c
#define ID_cv 0x0000d
#define ID_cfat_um 0x0000e
#define ID_ctfat 0x0000f
#define ID_cbfat 0x00010
#define ID_cskin_um 0x00011
#define ID_ctskin 0x00012
#define ID_cbskin 0x00013
#define ID_cmusc_um 0x00014
#define ID_ctmusc 0x00015
#define ID_cbmusc 0x00016
#define ID_cbone_um 0x00017
#define ID_ctbone 0x00018
#define ID_cbbone 0x00019
#define ID_cbrn_um 0x0001a
#define ID_ctbrn 0x0001b
#define ID_cbbrn 0x0001c
#define ID_clng_um 0x0001d
#define ID_ctlng 0x0001e
#define ID_cblng 0x0001f
#define ID_chrt_um 0x00020
#define ID_cthrt 0x00021
#define ID_cbhrt 0x00022
#define ID_ckdn_um 0x00023
#define ID_ctkdn 0x00024
#define ID_cbkdn 0x00025
#define ID_cgi_um 0x00026
#define ID_ctgi 0x00027
#define ID_cbgi 0x00028
#define ID_cliv_um 0x00029
#define ID_ctliv 0x0002a
#define ID_cbliv 0x0002b
#define ID_crpf_um 0x0002c
#define ID_ctrpf 0x0002d
#define ID_cbrpf 0x0002e
#define ID_cspf_um 0x0002f
#define ID_ctspf 0x00030
#define ID_cbspf 0x00031
#define ID_auc 0x00032
#define ID_InstInhDose 0x00033
#define ID_mbal 0x00034

/* Parameters */
static double parms[129];

#define mw parms[0]
#define bdose parms[1]
#define blen parms[2]
#define breps parms[3]
#define totbreps parms[4]
#define drdose parms[5]
#define vdw parms[6]
#define dreps parms[7]
#define inhdose parms[8]
#define inhtlen parms[9]
#define inhdays parms[10]
#define ivdose parms[11]
#define ivlen parms[12]
#define bw parms[13]
#define qcc parms[14]
#define hct parms[15]
#define vbldc parms[16]
#define perfc parms[17]
#define kbld parms[18]
#define respr parms[19]
#define tv parms[20]
#define ds parms[21]
#define uflw parms[22]
#define gfr parms[23]
#define frwsol parms[24]
#define fatvtbc parms[25]
#define vfatc parms[26]
#define qfatc parms[27]
#define pfat parms[28]
#define skinvtbc parms[29]
#define vskinc parms[30]
#define qskinc parms[31]
#define pskin parms[32]
#define muscvtbc parms[33]
#define vmuscc parms[34]
#define qmuscc parms[35]
#define pmusc parms[36]
#define bonevtbc parms[37]
#define vbonec parms[38]
#define qbonec parms[39]
#define pbone parms[40]
#define brnvtbc parms[41]
#define vbrnc parms[42]
#define qbrnc parms[43]
#define pbrn parms[44]
#define lngvtbc parms[45]
#define vlngc parms[46]
#define qlngc parms[47]
#define plng parms[48]
#define hrtvtbc parms[49]
#define vhrtc parms[50]
#define qhrtc parms[51]
#define phrt parms[52]
#define givtbc parms[53]
#define vgic parms[54]
#define qgic parms[55]
#define pgi parms[56]
#define fa parms[57]
#define ka parms[58]
#define livvtbc parms[59]
#define vlivc parms[60]
#define qalivc parms[61]
#define qvlivc parms[62]
#define pliv parms[63]
#define kdnvtbc parms[64]
#define vkdnc parms[65]
#define qkdnc parms[66]
#define pkdn parms[67]
#define rpfvtbc parms[68]
#define vrpfc parms[69]
#define qrpfc parms[70]
#define prpf parms[71]
#define spfvtbc parms[72]
#define vspfc parms[73]
#define qspfc parms[74]
#define pspf parms[75]
#define res parms[76]
#define fupls parms[77]
#define veh_flag parms[78]
#define kfec parms[79]
#define kVtoL parms[80]
#define kent parms[81]
#define vbld parms[82]
#define vpls parms[83]
#define vfat parms[84]
#define vskin parms[85]
#define vmusc parms[86]
#define vbone parms[87]
#define vbrn parms[88]
#define vlng parms[89]
#define vhrt parms[90]
#define vkdn parms[91]
#define vgi parms[92]
#define vliv parms[93]
#define vrpf parms[94]
#define vspf parms[95]
#define total_perf parms[96]
#define qcp parms[97]
#define qfat parms[98]
#define qskin parms[99]
#define qmusc parms[100]
#define qbone parms[101]
#define qbrn parms[102]
#define qlng parms[103]
#define qhrt parms[104]
#define qkdn parms[105]
#define qvliv parms[106]
#define qgi parms[107]
#define qaliv parms[108]
#define qrpf parms[109]
#define qspf parms[110]
#define pafat parms[111]
#define paskin parms[112]
#define pamusc parms[113]
#define pabone parms[114]
#define pabrn parms[115]
#define palng parms[116]
#define pahrt parms[117]
#define pakdn parms[118]
#define pagi parms[119]
#define paliv parms[120]
#define parpf parms[121]
#define paspf parms[122]
#define vkm1 parms[123]
#define vmaxliv parms[124]
#define km parms[125]
#define cinh parms[126]
#define qalv parms[127]
#define pair parms[128]

/* Forcing (Input) functions */
static double forc[0];


/* Function definitions for delay differential equations */

int Nout=1;
int nr[1]={0};
double ytau[1] = {0.0};

static double yini[48] = {0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}; /*Array of initial state variables*/

void lagvalue(double T, int *nr, int N, double *ytau) {
  static void(*fun)(double, int*, int, double*) = NULL;
  if (fun == NULL)
    fun = (void(*)(double, int*, int, double*))R_GetCCallable("deSolve", "lagvalue");
  return fun(T, nr, N, ytau);
}

double CalcDelay(int hvar, double dTime, double delay) {
  double T = dTime-delay;
  if (dTime > delay){
    nr[0] = hvar;
    lagvalue( T, nr, Nout, ytau );
}
  else{
    ytau[0] = yini[hvar];
}
  return(ytau[0]);
}

/*----- Initializers */
void initmod (void (* odeparms)(int *, double *))
{
  int N=129;
  odeparms(&N, parms);
}

void initforc (void (* odeforcs)(int *, double *))
{
  int N=0;
  odeforcs(&N, forc);
}


/* Calling R code will ensure that input y has same
   dimension as yini */
void initState (double *y)
{
  int i;

  for (i = 0; i < sizeof(yini) / sizeof(yini[0]); i++)
  {
    yini[i] = y[i];
  }
}

void getParms (double *inParms, double *out, int *nout) {
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

void derivs (int *neq, double *pdTime, double *y, double *ydot, double *yout, int *ip)
{
  /* local */ double dose_in_gut;
  /* local */ double available_dose;
  /* local */ double raabsgut;
  /* local */ double rinh;
  /* local */ double rexh;
  /* local */ double riv;
  /* local */ double rauexc;
  /* local */ double rametliv1;
  /* local */ double rametliv2;
  /* local */ double raclbld;
  /* local */ double totdose;
  /* local */ double totbody;
  /* local */ double totclear;
  /* local */ double tmass;

  yout[ID_cpls] = y[ID_abld] / vpls ;

  yout[ID_auc] = y[ID_aucpls] ;

  yout[ID_ctfat] = y[ID_atfat] / ( fatvtbc * vfat ) ;

  yout[ID_cbfat] = y[ID_abfat] / ( ( 1 - fatvtbc ) * vfat ) ;

  yout[ID_afat] = y[ID_atfat] + y[ID_abfat] ;

  yout[ID_cfat_um] = ( y[ID_atfat] + y[ID_abfat] ) / vfat ;

  yout[ID_ctskin] = y[ID_atskin] / ( skinvtbc * vskin ) ;

  yout[ID_cbskin] = y[ID_abskin] / ( ( 1 - skinvtbc ) * vskin ) ;

  yout[ID_askin] = y[ID_atskin] + y[ID_abskin] ;

  yout[ID_cskin_um] = ( y[ID_atskin] + y[ID_abskin] ) / vskin ;

  yout[ID_ctmusc] = y[ID_atmusc] / ( muscvtbc * vmusc ) ;

  yout[ID_cbmusc] = y[ID_abmusc] / ( ( 1 - muscvtbc ) * vmusc ) ;

  yout[ID_amusc] = y[ID_atmusc] + y[ID_abmusc] ;

  yout[ID_cmusc_um] = ( y[ID_atmusc] + y[ID_abmusc] ) / vmusc ;

  yout[ID_ctbone] = y[ID_atbone] / ( bonevtbc * vbone ) ;

  yout[ID_cbbone] = y[ID_abbone] / ( ( 1 - bonevtbc ) * vbone ) ;

  yout[ID_abone] = y[ID_atbone] + y[ID_abbone] ;

  yout[ID_cbone_um] = ( y[ID_atbone] + y[ID_abbone] ) / vbone ;

  yout[ID_ctbrn] = y[ID_atbrn] / ( brnvtbc * vbrn ) ;

  yout[ID_cbbrn] = y[ID_abbrn] / ( ( 1 - brnvtbc ) * vbrn ) ;

  yout[ID_abrn] = y[ID_atbrn] + y[ID_abbrn] ;

  yout[ID_cbrn_um] = ( y[ID_atbrn] + y[ID_abbrn] ) / vbrn ;

  yout[ID_ctlng] = y[ID_atlng] / ( lngvtbc * vlng ) ;

  yout[ID_cblng] = y[ID_ablng] / ( ( 1 - lngvtbc ) * vlng ) ;

  yout[ID_alng] = y[ID_atlng] + y[ID_ablng] ;

  yout[ID_clng_um] = ( y[ID_atlng] + y[ID_ablng] ) / vlng ;

  yout[ID_cthrt] = y[ID_athrt] / ( hrtvtbc * vhrt ) ;

  yout[ID_cbhrt] = y[ID_abhrt] / ( ( 1 - hrtvtbc ) * vhrt ) ;

  yout[ID_ahrt] = y[ID_athrt] + y[ID_abhrt] ;

  yout[ID_chrt_um] = ( y[ID_athrt] + y[ID_abhrt] ) / vhrt ;

  yout[ID_ctkdn] = y[ID_atkdn] / ( kdnvtbc * vkdn ) ;

  yout[ID_cbkdn] = y[ID_abkdn] / ( ( 1 - kdnvtbc ) * vkdn ) ;

  yout[ID_akdn] = y[ID_atkdn] + y[ID_abkdn] ;

  yout[ID_ckdn_um] = ( y[ID_atkdn] + y[ID_abkdn] ) / vkdn ;

  yout[ID_ctgi] = y[ID_atgi] / ( givtbc * vgi ) ;

  yout[ID_cbgi] = y[ID_abgi] / ( ( 1 - givtbc ) * vgi ) ;

  yout[ID_agi] = y[ID_atgi] + y[ID_abgi] ;

  yout[ID_cgi_um] = ( y[ID_atgi] + y[ID_abgi] ) / vgi ;

  yout[ID_ctliv] = y[ID_atliv] / ( livvtbc * vliv ) ;

  yout[ID_cbliv] = y[ID_abliv] / ( ( 1 - livvtbc ) * vliv ) ;

  yout[ID_aliv] = y[ID_atliv] + y[ID_abliv] ;

  yout[ID_cliv_um] = ( y[ID_atliv] + y[ID_abliv] ) / vliv ;

  yout[ID_ctrpf] = y[ID_atrpf] / ( rpfvtbc * vrpf ) ;

  yout[ID_cbrpf] = y[ID_abrpf] / ( ( 1 - rpfvtbc ) * vrpf ) ;

  yout[ID_arpf] = y[ID_atrpf] + y[ID_abrpf] ;

  yout[ID_crpf_um] = ( y[ID_atrpf] + y[ID_abrpf] ) / vrpf ;

  yout[ID_ctspf] = y[ID_atspf] / ( spfvtbc * vspf ) ;

  yout[ID_cbspf] = y[ID_abspf] / ( ( 1 - spfvtbc ) * vspf ) ;

  yout[ID_aspf] = y[ID_atspf] + y[ID_abspf] ;

  yout[ID_cspf_um] = ( y[ID_atspf] + y[ID_abspf] ) / vspf ;

  yout[ID_cv] = ( qfat * yout[ID_cbfat] + qskin * yout[ID_cbskin] + qmusc * yout[ID_cbmusc] + qbone * yout[ID_cbbone] + qbrn * yout[ID_cbbrn] + qlng * yout[ID_cblng] + qhrt * yout[ID_cbhrt] + qkdn * yout[ID_cbkdn] + qvliv * yout[ID_cbliv] + qrpf * yout[ID_cbrpf] + qspf * yout[ID_cbspf] ) / qcp ;

  ydot[ID_odose] = 0 ;

  ydot[ID_ddose] = 0 ;

  ydot[ID_odose1] = - ka * y[ID_odose1] ;

  ydot[ID_ddose1] = - ka * y[ID_ddose1] ;

  ydot[ID_odose2] = - kfec * y[ID_odose2] - kVtoL * y[ID_odose2] ;

  ydot[ID_ddose2] = - kfec * y[ID_ddose] - kVtoL * y[ID_ddose2] ;

  ydot[ID_aLAS] = kVtoL * ( y[ID_odose2] + y[ID_ddose2] ) - ka * y[ID_aLAS] - kfec * y[ID_aLAS] - kent * y[ID_aLAS] ;

  ydot[ID_akent] = - kent * y[ID_aLAS] ;

  ydot[ID_afec] = - kfec * ( y[ID_odose2] + y[ID_ddose2] + y[ID_aLAS] ) ;

  ydot[ID_totodose] = 0 ;

  ydot[ID_totddose] = 0 ;

  dose_in_gut = ka * ( y[ID_odose1] + y[ID_ddose1] ) ;

  available_dose = dose_in_gut ;

  raabsgut = ka * y[ID_aLAS] + fa * available_dose ;

  ydot[ID_aabsgut] = raabsgut ;

  ydot[ID_anabsgut] = ( 1 - fa ) * available_dose ;

  ydot[ID_inhswch] = 0 ;

  rinh = y[ID_inhswch] * qalv * cinh ;

  ydot[ID_ainh] = rinh ;

  rexh = y[ID_inhswch] * qalv * yout[ID_cpls] / pair ;

  ydot[ID_aexh] = rexh ;

  ydot[ID_ivswch] = 0 ;

  riv = y[ID_ivswch] * ivdose ;

  ydot[ID_aiv] = riv ;

  ydot[ID_abfat] = qfat * yout[ID_cpls] - qfat * yout[ID_cbfat] + pafat * yout[ID_ctfat] / pfat - pafat * yout[ID_cbfat] ;

  ydot[ID_atfat] = pafat * yout[ID_cbfat] - pafat * yout[ID_ctfat] / pfat ;

  ydot[ID_abskin] = qskin * yout[ID_cpls] - qskin * yout[ID_cbskin] + paskin * yout[ID_ctskin] / pskin - paskin * yout[ID_cbskin] ;

  ydot[ID_atskin] = paskin * yout[ID_cbskin] - paskin * yout[ID_ctskin] / pskin ;

  ydot[ID_abmusc] = qmusc * yout[ID_cpls] - qmusc * yout[ID_cbmusc] + pamusc * yout[ID_ctmusc] / pmusc - pamusc * yout[ID_cbmusc] ;

  ydot[ID_atmusc] = pamusc * yout[ID_cbmusc] - pamusc * yout[ID_ctmusc] / pmusc ;

  ydot[ID_abbone] = qbone * yout[ID_cpls] - qbone * yout[ID_cbbone] + pabone * yout[ID_ctbone] / pbone - pabone * yout[ID_cbbone] ;

  ydot[ID_atbone] = pabone * yout[ID_cbbone] - pabone * yout[ID_ctbone] / pbone ;

  ydot[ID_abbrn] = qbrn * yout[ID_cpls] - qbrn * yout[ID_cbbrn] + pabrn * yout[ID_ctbrn] / pbrn - pabrn * yout[ID_cbbrn] ;

  ydot[ID_atbrn] = pabrn * yout[ID_cbbrn] - pabrn * yout[ID_ctbrn] / pbrn ;

  ydot[ID_ablng] = qlng * yout[ID_cpls] - qlng * yout[ID_cblng] + palng * yout[ID_ctlng] / plng - palng * yout[ID_cblng] + rinh - rexh ;

  ydot[ID_atlng] = palng * yout[ID_cblng] - palng * yout[ID_ctlng] / plng ;

  ydot[ID_abhrt] = qhrt * yout[ID_cpls] - qhrt * yout[ID_cbhrt] + pahrt * yout[ID_cthrt] / phrt - pahrt * yout[ID_cbhrt] ;

  ydot[ID_athrt] = pahrt * yout[ID_cbhrt] - pahrt * yout[ID_cthrt] / phrt ;

  rauexc = gfr * frwsol * ( 1 - res ) * yout[ID_ctkdn] / pkdn ;

  ydot[ID_auexc] = rauexc ;

  ydot[ID_abkdn] = qkdn * yout[ID_cpls] - qkdn * yout[ID_cbkdn] + pakdn * yout[ID_ctkdn] / pkdn - pakdn * yout[ID_cbkdn] ;

  ydot[ID_atkdn] = pakdn * yout[ID_cbkdn] - pakdn * yout[ID_ctkdn] / pkdn - rauexc ;

  ydot[ID_abgi] = qgi * yout[ID_cpls] - qgi * yout[ID_cbgi] + pagi * yout[ID_ctgi] / pgi - pagi * yout[ID_cbgi] + raabsgut ;

  ydot[ID_atgi] = pagi * yout[ID_cbgi] - pagi * yout[ID_ctgi] / pgi ;

  rametliv1 = vkm1 * ( yout[ID_ctliv] * fupls / pliv ) ;

  ydot[ID_ametliv1] = rametliv1 ;

  rametliv2 = vmaxliv * ( yout[ID_ctliv] * fupls / pliv ) / ( ( yout[ID_ctliv] / pliv ) + km ) ;

  ydot[ID_ametliv2] = rametliv2 ;

  ydot[ID_abliv] = qaliv * yout[ID_cpls] - qvliv * yout[ID_cbliv] + qgi * yout[ID_cbgi] + paliv * yout[ID_ctliv] / pliv - paliv * yout[ID_cbliv] ;

  ydot[ID_atliv] = paliv * yout[ID_cbliv] - paliv * yout[ID_ctliv] / pliv - rametliv1 - rametliv2 ;

  ydot[ID_abrpf] = qrpf * yout[ID_cpls] - qrpf * yout[ID_cbrpf] + parpf * yout[ID_ctrpf] / prpf - parpf * yout[ID_cbrpf] ;

  ydot[ID_atrpf] = parpf * yout[ID_cbrpf] - parpf * yout[ID_ctrpf] / prpf ;

  ydot[ID_abspf] = qspf * yout[ID_cpls] - qspf * yout[ID_cbspf] + paspf * yout[ID_ctspf] / pspf - paspf * yout[ID_cbspf] ;

  ydot[ID_atspf] = paspf * yout[ID_cbspf] - paspf * yout[ID_ctspf] / pspf ;

  raclbld = kbld * yout[ID_cpls] * fupls ;

  ydot[ID_aclbld] = raclbld ;

  ydot[ID_abld] = qcp * ( yout[ID_cv] - yout[ID_cpls] ) + riv - raclbld ;

  ydot[ID_aucpls] = yout[ID_cpls] ;

  totdose = y[ID_totodose] + y[ID_totddose] + y[ID_ainh] + y[ID_aiv] ;

  totbody = y[ID_abld] + yout[ID_afat] + yout[ID_askin] + yout[ID_amusc] + yout[ID_abone] + yout[ID_abrn] + yout[ID_alng] + yout[ID_ahrt] + yout[ID_agi] + yout[ID_aliv] + yout[ID_akdn] + yout[ID_arpf] + yout[ID_aspf] + y[ID_odose1] + y[ID_ddose1] + y[ID_odose2] + y[ID_ddose2] + y[ID_aLAS] ;

  totclear = y[ID_ametliv1] + y[ID_ametliv2] + y[ID_aclbld] + y[ID_auexc] + y[ID_anabsgut] + y[ID_aexh] + y[ID_akent] + y[ID_afec] ;

  tmass = totdose - totbody - totclear ;

  yout[ID_InstInhDose] = rinh ;
  yout[ID_mbal] = tmass ;

} /* derivs */


/*----- Jacobian calculations: */
void jac (int *neq, double *t, double *y, int *ml, int *mu, double *pd, int *nrowpd, double *yout, int *ip)
{

} /* jac */


/*----- Events calculations: */
void event (int *n, double *t, double *y)
{

  y[ID_odose] = ( bdose > 0 ? y[ID_odose] + ( bdose * bw * 1000. / mw ) / totbreps : y[ID_odose] ) ;
  y[ID_totodose] = ( bdose > 0 ? y[ID_totodose] + ( bdose * bw * 1000. / mw ) / totbreps : y[ID_totodose] ) ;

  y[ID_ddose] = ( drdose > 0 ? y[ID_ddose] + ( drdose * 1000. * vdw / mw ) / dreps : y[ID_ddose] ) ;
  y[ID_totddose] = ( drdose > 0 ? y[ID_totddose] + ( drdose * 1000. * vdw / mw ) / dreps : y[ID_totddose] ) ;

  y[ID_odose1] = ( veh_flag == 0 ? y[ID_odose] : 0 ) ;
  y[ID_odose2] = ( veh_flag == 1 ? y[ID_odose] : 0 ) ;
  y[ID_ddose1] = ( veh_flag == 0 ? y[ID_ddose] : 0 ) ;
  y[ID_ddose2] = ( veh_flag == 1 ? y[ID_ddose] : 0 ) ;

  y[ID_inhswch] = ( inhdose > 0 ? ( y[ID_inhswch] == 0 ? 1 : 0 ) : 0 ) ;
  y[ID_ivswch] = ( ivdose > 0 ? ( y[ID_ivswch] == 0 ? 1 : 0 ) : 0 ) ;

} /* event */

/*----- Roots calculations: */
void root (int *neq, double *t, double *y, int *ng, double *gout, double *out, int *ip)
{

} /* root */

