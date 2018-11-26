## Resubmission
This is a resubmission and makes the following changes
1) Examples have been added to main functions that launch shiny interfaces to run the analysis. They are defined as dontrun.
2) A sentence has been added to the function description and package description informing the users that all analysis can only be run using the user interface.

## Resubmission
This is a resubmision and makes the following changes based on reviewer comments:
1) Space added after periods in package description
2) Acronyms and package names enclosed by single quotes instead of double quotes
3) Vignette added that explains basic setup needed to start using the package. Since all functions in the package are run through the interactive Shiny interface, the vignette does not actually execute any code.


## Resbumission
This is a resubmission. The last version submitted had a period missing at the end of the description. Causing it to error out. This version is spell checked for accuracy. 

## Resubmission
This is a resubmission. In this version I have addressed two concerns raised during the previous review. Specifically

* No packages are a part of the Depends field in the description. The namespace and imports were reorganised to ensure the package still works
* The description has been updated.

## Resubmission
This is a resubmission. In this version I have fixed the two warnings that were a part of the previous submission. This version now has

* Import functions corrected to avoid replacing previously imported functions
* Complete documentation for all exported functions. 

## Test environments
* local OS X install, R 3.5.0
* local windows install
* win-builder (devel and release)

## R CMD check results

0 errors | 2 warnings | 1 note

* checking whether package 'plethem' can be installed ... WARNING
  Warning: replacing previous import 'shiny::runExample' by 'shinyjs::runExample' when loading 'plethem'
  Warning: replacing previous import 'shiny::dataTableOutput' by 'DT::dataTableOutput' when loading   'plethem'
  This is expected and agreeable since the function from DT improves upon the function from Shiny.

* checking Rd \usage sections ... WARNING
  Certain functions have undocumented arguments. These functions are external modules that run the user interface. They need to be exported for the UI to work, but should not be called by the users. Their description and title explains that clearly.
  
* checking package dependencies ... NOTE
Depends: includes the non-default packages:
  'shiny' 'shinyjs' 'ggplot2' 'V8' 'shinyBS' 'shinyWidgets' 'httk'
  'miniUI' 'shinydashboard' 'rstudioapi'
Adding so many packages to the search path is excessive and importing
selectively is preferable.

*

* This is a new release.

## Reverse dependencies

This is a new release, so there are no reverse dependencies.

