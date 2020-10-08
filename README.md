## PLETHEM User Guide.
A step by step user guide for the current version of PLETHEM can be found under inst/docs folder in the repo above.

## PLETHEM INSTALLATION

### Installing R.
Installing PLETHEM requires the latest version of 64 bit R-4.0.2 ([link](https://cran.r-project.org/bin/windows/base/)), RStudio ([link](https://rstudio.com/)), and Rtools([link](https://cran.r-project.org/bin/windows/Rtools/)).
While installing Rtools make sure the box that says "Add Rtools to System Path" is checked.

### Installing the package
Open Rstudio once all of the above are installed. On the Rstudio console type the following command
---
  >install.packages("devtools",dependencies = T)
  
This will install the package "devtools". This package is needed to install PLETHEM from the repos
Then type one of the following commands

  >devtools::install_github("ScitoVation/plethem")
  
to install from Github.

Plethem may request to install packages into your R library. Select 1 to install all packages and click no if asked "Do you want to install packages from source...".
You may need to install the backports package seperately. (If this happens you will need to reinstall plethem from github as well.) To do this run

  >install.packages("backports")
  
 Select yes if prompted to restart R.
 Select no if asked "Do you want to install packages from source...".
 
 Run the PLETHEM install again
 
   >devtools::install_github("ScitoVation/plethem")
   
 When prompted to install packages, hit enter, do not type a number.

## Initial Setup

PLETHEM pulls data from three databases. The master database is installed with the package and cannot be modified by the user. The User database is a local copy that is a subset of the master database. This table is used to store user specific parameters that may be needed across different projects (Like a set of chemicals that are frequently used by the user but are not a part of the main database). The final database is project database. This is used by PLETHEM internally to keep track of all the data while the used is working on a specific project in PLETHEM. When the project is closed, the database is saved out as an .Rproj file that can be used to recreate the project on any other PLETHEM installation. 

To complete the initial setup, the user needs to define the location where they want to store the user database. If they are updating a previous installation of PLETHEM or want to use the user database from another installation, they can just point to the database file instead. To either create a new database or to use an existing file run the following command and follow the prompts that come up
  >library(plethem)
  To use an exiting database, click on "Select user database" from the Addins menu or type
  >setUserDb()
  To create a new database, click on "Create new user database" from the Addins menu or type
  >createUserDb()
  
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
