## Resubmission
This is a resubmission. Fixed 1 warning and 2 notes.
* Warning related to incorrect C code in the package is fixed
* Note related to no visible global function definition fixed
* Note related to 'data.table' appearing two times in Description fixed
* Also fixed minor issue with description definition that were not flagged yet

## Test environments
* local OS X install, R 4.0.0
* local windows 10 install, R 4.0.0
* local windows 10 install, R 4.0.1
* local windows 7 install, R 3.6.0

## R CMD check results Windows

*checking R files for syntax errors ... WARNING
  Warnings in file 'R/HT_IVIVE_InputModule.R':
    unable to translate '<U+00B5>M' to native encoding
    unable to translate '<U+00B5>mol/min/mg Protein' to native encoding
    unable to translate '<U+00B5>L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>L/h/mg Protein' to native encoding
    unable to translate '<U+00B5>L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>L/h/mg Protein' to native encoding
    unable to translate '<U+00B5>L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>L/h/mg Protein' to native encoding
  
  Warnings in file 'R/performIVIVEModule.R':
    unable to translate '<U+00B5>  mol/min/10^6 hepatocytes' to native encoding
    unable to translate '<U+00B5>  mol/min/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/h/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/h/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/min/mg Protein' to native encoding
    unable to translate '<U+00B5>  L/h/mg Protein' to native encoding

* checking package dependencies ... NOTE
  Imports includes 32 non-default packages.
  Importing from so many packages makes the package vulnerable to any of
  them becoming unavailable.  Move as many as possible to Suggests and
  use conditionally.

* checking R code for possible problems ... NOTE
  runFDPBPK: no visible global function definition for
    'fishPBPK_initStates'
  runFDPBPK: no visible global function definition for
    'fishPBPK_initParms'
  runFDPBPK: no visible binding for global variable 'fishPBPK_Outputs'
  Undefined global functions or variables:
    fishPBPK_Outputs fishPBPK_initParms fishPBPK_initStates

* checking package vignettes in 'inst/doc' ... NOTE
  Package vignette with placeholder title 'Vignette Title':
    'getting_started.Rmd'

* checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    'examples_i386' 'examples_x64' 'plethem-Ex_i386.Rout'
    'plethem-Ex_x64.Rout' 'tests_i386' 'tests_x64'

0 errors| 1 warning | 4 notes

## R CMD check results OSX

* checking package dependencies ... NOTE
  Package suggested but not available for checking: ‘rmarkdown’


*  Imports includes 32 non-default packages.
Importing from so many packages makes the package vulnerable to any of them becoming unavailable.  Move as many as possible to Suggests and use conditionally.


* checking R code for possible problems ... NOTE
  runFDPBPK: no visible global function definition for
    ‘fishPBPK_initStates’
  runFDPBPK: no visible global function definition for ‘fishPBPK_initParms’
  runFDPBPK: no visible binding for global variable ‘fishPBPK_Outputs’
  Undefined global functions or variables:
    fishPBPK_Outputs fishPBPK_initParms fishPBPK_initStates

* checking package vignettes in ‘inst/doc’ ... NOTE
  Package vignette with placeholder title ‘Vignette Title’:
    ‘getting_started.Rmd’


*0 errors| 0 warnings|3 notes

* This is a new release. The old version of the package is no longer available on CRAN

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

