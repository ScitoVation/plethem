.onAttach <- function(libname,pkgname){

  packageStartupMessage("You are now using PLETHEM")

}
release_questions <- function() {
  c(
    "Have you set the run_type to 'prod' in all server.R files?"
  )
}