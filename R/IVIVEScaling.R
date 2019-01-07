# the file contains all the functions required for IVIVE Scaling.


#' Scale clearance by age when both cellular and enzymatic clearance are known at referance age.
#' @description This function is used internally to calculate age specific metabolism using the IVIVE gadget.
#' It needs both cellular and enzymatic clearance at atleast one age, the reference age, to extrapolate to values at other ages.
#' @param out_ages Ages for which the clearance needs to be calculated including reference age
#' @param tot_scaled_hepcl Total cellular clearance at reference age in L/h/kg Liver
#' @param tot_scaled_recomcl Total Recombinant enzyme clearance at reference age in L/h/kg Liver
#' @param cypDb Dataframe containing cyp datasets to scale measured clearance values from in-vitro to in-vivo
#' @param cypCl Datafarame containing measured invitro enzymatic clearance.
#' @param gender Gender either "M" for male or "F" for female
#' @return List with individual enzyme and total clearance at all ages.
scale_cellular_enzymatic<- function(out_ages,tot_scaled_hepcl,tot_scaled_recomcl,cypDb,cypCl,gender){
  agewise_enzyme_cl <- scale_enzymatic(out_ages,
                                       tot_scaled_recomcl,
                                       cypDb,
                                       cypCl,
                                       gender)
  tot_agewise_enzyme_cl <- agewise_enzyme_cl["Total",]
  # if(length(tot_agewise_enzyme_cl)==1){
  #   tot_agewise_enzyme_cl<- list(tot_agewise_enzyme_cl)
  # }
  agewise_enzyme_cl<- agewise_enzyme_cl[!(row.names(agewise_enzyme_cl) == "Total"),]
  if (is.null(dim(agewise_enzyme_cl))){
    agewise_enzyme_cl <- array(agewise_enzyme_cl,dim = c(10,1))
  }
  # get the final age based clearance by scaling the enzymatic clearance with cellular and enzymatic clearance at referance age
  tot_agewise_scaled_cl <- tot_scaled_hepcl/(tot_scaled_recomcl/tot_agewise_enzyme_cl)
  # get fractional contribution of each enzyme
  fraction_contrib<- sweep(agewise_enzyme_cl,2,unlist(tot_agewise_enzyme_cl),"/")
  # use fraction contribution to get final contribution of each enzyme across ages
  agewise_scaled_enzyme_cl <- sweep(fraction_contrib,2,tot_agewise_scaled_cl,"*")
  agewise_scaled_enzyme_cl <- rbind(agewise_scaled_enzyme_cl,"Total"= tot_agewise_scaled_cl)
  return(agewise_scaled_enzyme_cl)
}


#' Scale clearance by age for when enzymatic clearance is known at referance age.
#' @param out_ages out_ages Ages for which the clearance needs to be calculated including reference age
#' @param tot_scaled_recomcl Total Recombinant enzyme clearance at reference age in L/h/kg Liver
#' @param cypDb Dataframe containing cyp datasets to scale measured clearance values from in-vitro to in-vivo
#' @param cypCl Datafarame containing measured incitro enzymatic clearance.
#' @param gender Gender, either "M" for male or "F" for female. Needed to get Liver weight
scale_enzymatic<- function(out_ages,tot_scaled_recomcl,cypDb,cypCl,gender){
  liver_wt_list <- lapply(out_ages,getLifecourseLiverVolume,gender)
  names(liver_wt_list)<- out_ages
  agewise_enzyme_cl <- mapply(calculateRecombClearance,
                               "liver_wt" = liver_wt_list,"age"=out_ages,
                               MoreArgs = list(clearance = cypCl,
                                               organism="human",
                                               cyp_data=cypDb,
                                               return_total = F))
  # get the clearance in l/h/ kgliver
  agewise_enzyme_cl <- sweep(agewise_enzyme_cl,2,unlist(liver_wt_list),"/")
  # get the total enzyme based clearance for each age
  tot_agewise_enzyme_cl <- apply(agewise_enzyme_cl,2,sum)
  agewise_enzyme_cl <- rbind(agewise_enzyme_cl,"Total"= tot_agewise_enzyme_cl)
  return(agewise_enzyme_cl)
}
