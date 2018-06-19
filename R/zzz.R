.onAttach <- function(libname,pkgname){
  packageStartupMessage("You are now using PLETHEM. 
                        If you have installed PLETHEM for the first time or have just updated it, please run setUserDb() to select an existing database or createUserDb() to create a new one.")
}