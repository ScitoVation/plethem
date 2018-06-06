## PLETHEM INSTALLATION

### Installing R.
Installing PLETHEM requires the latest version of 64 bit R-3.4.4 or higher([link](https://www.r-project.org/)), RStudio([link](https://www.rstudio.com/)), and Rtools([link](https://cran.r-project.org/bin/windows/Rtools/))

### Installing the package
Open Rstudio once all of the above are installed. On the Rstudio console type the following command
---
  >install.packages("devtools",dependencies = T)lilib
  
This will install the package "devtools". This package is needed to install PELTHEM from the repos
Then type one of the following commands
  >devtools::install_github("ScitoVation/plethem")
  
to install form Github or 
  > devtools::install_bitbucket("ScivCompTox/plethem")

to install from BitBucket.

## Initial Setup

PLETHEM pulls data from three databases. The master database is installed with the package and cannot be modified by the user. The User database is a local copy that is a subset of the master database. This table is used to store user specific parameters that may be needed across different projects (Like a set of chemicals that are frequently used by the user but are not a part of the main database). The final database is project database. This is used by PLETHEM internally to keep track of all the data while the used is working on a specific project in PLETHEM. When the project is closed, the database is saved out as an .Rproj file that can be used to recreate the project on any other PLETHEM installation. 

To complete the initial setup, the user needs to define the location where they want to store the user database. If they are updating a previous installation of PLETHEM or want to use the user database from another installation, they can just point to the database file instead. To either create a new database or to use an existing file run the following command and follow the prompts that come up
  >setUserDb()
  
This only needs to be done once.

## PBPK modeling in PLETHEM
### Load PLETHEM and start a project.
To load plethem and start a new PBPK modeling project 

 >library(plethem)
 >newProject("Name of the project")
 
This will bring up a popup window where you can select where you want to save the project files. Once you have selected the directory here, plethem will launch the PBPK interface in your default browser. If the interfaces launches in a popup window instead, just click "Open in Browser" button on the window.

On exit, plethem will save all the data as a .Rdata file in the folder originally selected

### Load existing Projects
To reload previously created projects just enter the following on the console.
 >loadProject()
 
This would popup an explorer window where you can select the .Rdata project file of any previously stored project. On exiting the interface, any changes made will overwrite this file. 
