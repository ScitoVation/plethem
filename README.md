## PLETHEM INSTALLATION

### Installing R.
Installing PLETHEM requires the latest version of 64 bit R-3.4.4 or higher[(link)](https://www.r-project.org/), RStudio[(link)](https://www.rstudio.com/), and Rtools[(link)](https://cran.r-project.org/bin/windows/Rtools/)

### Installing the package
Open Rstudio once all of the above are installed. On the Rstudio console type the following command
```r
install.packages("devtools")
```
This will install the package "devtools". This package is needed to install PELTHEM from the repos
Then type one of the following commands
```r
devtools::install_github("ScitoVation/plethem")
``` 
to install form Github or 
```r
devtools::install_bitbucket("ScivCompTox/plethem")
``` 
to install from BitBucket.

## PBPK modeling in PLETHEM
### Load PLETHEM and start a project.
To load plethem and start a new PBPK modeling project 
```r
library(plethem)
newProject("Name of the project")
```
This will bring up a popup window where you can select where you want to save the project files. Once you have selected the directory here, plethem will launch the PBPK interface in your default browser. If the interfaces launches in a popup window instead, just click "Open in Browser" button on the window.

On exit, plethem will save all the data as a .Rdata file in the folder originally selected

### Load existing Projects
To reload previously created projects just enter the following on the console.
```r
loadProject()
```
This would popup an explorer window where you can select the .Rdata project file of any previously stored project. On exiting the interface, any changes made will overwrite this file. 
