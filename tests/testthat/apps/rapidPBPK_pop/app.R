library(plethem)

clearProjectDb()

# load project and reset database
fpath <- system.file(package = "plethem", "extdata/Demo_Project.Rdata")
# 

loadProject(file_path=fpath, runUI=F)

query <- "Update Utils Set Value=NULL;"
mainDbUpdate(query)

shinyAppDir(system.file('rapidPBPK_pop',package="plethem"))
# js$reset()