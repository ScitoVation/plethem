# This file contains utility functions for PBPK models
# none of these functions should be called by the user

#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
createPBPKflowchart <- function(){
  flowChartString <- "digraph {

graph [layout = dot, rankdir = LR]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

skinSurface [label = 'Skin\nSurface', shape = oval, fillcolor = yellowGreen]
dermal [label = 'Dermal', shape = oval, fillcolor = yellowGreen]
inhalation [label = 'Inhalation', shape = oval, fillcolor = yellowGreen]
skin [label = 'Skin', shape = oval, fillcolor = Purple]
fat [label = 'Fat', shape = oval, fillcolor = Purple]
richlyPerfused [label = 'Richly\nPerfused', shape = oval, fillcolor = Purple]
slowlyPerfused [label = 'Slowly\nPerfused', shape = oval, fillcolor = Purple]
liver [label = 'Liver', shape = oval, fillcolor = Purple]
oral [label = 'Oral', shape = oval, fillcolor = yellowGreen]
venousBlood [label = 'Venous\nBlood', shape = oval, fillcolor = Blue]
arterialBlood [label = 'Arterial\nBlood', shape = oval, fillcolor = Red]
liver2 [label = 'Liver', shape = oval, fillcolor = forestGreen]
restOfBody [label = 'Rest\nof\nBody', shape = oval, fillcolor = forestGreen]
kidney [label = 'Kidney', shape = oval, fillcolor = forestGreen]
urine [label = 'Urine' shape = oval, fillcolor = Orange]


# edge definitions with the node IDs
dermal -> skinSurface -> skin
inhalation -> arterialBlood -> {skin fat richlyPerfused slowlyPerfused liver} -> venousBlood -> arterialBlood
oral -> liver -> liver2 -> restOfBody -> kidney -> urine
kidney -> restOfBody -> liver2
}"
  
  return(flowChartString)
}



#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
addPBPKequations <- function(){
 PBPKequations <- "" 
}

#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
createHESIgraphs <- function(){
  PBPKequations <- "" 
}