# This file contains utility functions for PBPK models
# none of these functions should be called by the user

#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
createPBPKflowchart <- function(HESI_doc){
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
  
  # produce an emf file containing the 
  flowChart43 <- tempfile(fileext = ".png")
  grViz(flowChartString) %>%
    export_svg %>% charToRaw %>% rsvg_png(flowChart43)
  HESI_doc %>%
  cursor_reach('^Model Development and Structure$') %>%
    cursor_forward() %>%
    body_add_img(src = flowChart43, width = 5, height = 2) %>%
    body_add_par("Flow Chart of PBPK Model", style="Normal")
  
  return(HESI_doc)
}



#' Gets the metabolism data. Should not be used by directly by the user
#' @description The function returns the relavent metabolism data if the simulation contains
#' data from the metabolism set
#' @param admeid The id for ADME set. The admeid is used to obtain information about the other sets.
#' @return List containing the metabolism values needed to run PBPK model or
#' display simulation information
#' @export
addPBPKequations <- function(HESI_doc){
  HESI_doc %>%
    cursor_reach('^Model Equations$') %>%
    cursor_forward() %>%
    body_add_par( style="Normal") %>%
    body_add_par("### CONCENTRATION CALCULATIONS ###", style="Normal") %>%
    body_add_par("Blood Plasma (arterial)", style="Normal") %>%
    body_add_par("cpls = abld/vpls", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Fat", style="Normal") %>%
    body_add_par("ctfat = atfat/(fatvtbc*vfat)           # fat Tissue Concentration", style="Normal") %>%
    body_add_par("cbfat = abfat/((1-fatvtbc)*vfat)       # fat Blood Concentration", style="Normal") %>%
    body_add_par("afat = atfat + abfat", style="Normal") %>%
    body_add_par("cfat_um = (atfat+abfat)/vfat", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Skin", style="Normal") %>%
    body_add_par("ctskin = atskin/(skinvtbc*vskin)       # Skin Tissue Concentration", style="Normal") %>%
    body_add_par("cbskin = abskin/((1-skinvtbc)*vskin)   # SKin Blood Concentration", style="Normal") %>%
    body_add_par("askin = atskin+abskin", style="Normal") %>%
    body_add_par("cskin_um = (atskin+abskin)/vskin", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Muscle", style="Normal") %>%
    body_add_par("ctmusc = atmusc/(muscvtbc*vmusc)       # Muscle Tissue Concentration", style="Normal") %>%
    body_add_par("cbmusc = abmusc/((1-muscvtbc)*vmusc)   # Muscle Blood Concentration", style="Normal") %>%
    body_add_par("amusc = atmusc+abmusc", style="Normal") %>%
    body_add_par("cmusc_um = (atmusc+abmusc)/vmusc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Bone", style="Normal") %>%
    body_add_par("ctbone = atbone/(bonevtbc*vbone)       # Bone Tissue Concentration", style="Normal") %>%
    body_add_par("cbbone = abbone/((1-bonevtbc)*vbone)   # Bone Blood Concentration", style="Normal") %>%
    body_add_par("abone = atbone+abbone", style="Normal") %>%
    body_add_par("cbone_um = (atbone+abbone)/vbone", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Brain", style="Normal") %>%
    body_add_par("ctbrn = atbrn/(brnvtbc*vbrn)       # Brain Tissue Concentration", style="Normal") %>%
    body_add_par("cbbrn = abbrn/((1-brnvtbc)*vbrn)   # Brain Blood Concentration", style="Normal") %>%
    body_add_par("abrn = atbrn+abbrn", style="Normal") %>%
    body_add_par("cbrn_um = (atbrn+abbrn)/vbrn", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Lung", style="Normal") %>%
    body_add_par("ctlng = atlng/(lngvtbc*vlng)       # Lung Tissue Concentration", style="Normal") %>%
    body_add_par("cblng = ablng/((1-lngvtbc)*vlng)   # Lung Blood Concentration", style="Normal") %>%
    body_add_par("alng = atlng+ablng", style="Normal") %>%
    body_add_par("clng_um = (atlng+ablng)/vlng", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Heart", style="Normal") %>%
    body_add_par("cthrt = athrt/(hrtvtbc*vhrt)       # Heart Tissue Concentration", style="Normal") %>%
    body_add_par("cbhrt = abhrt/((1-hrtvtbc)*vhrt)   # Heart Blood Concentration", style="Normal") %>%
    body_add_par("ahrt = athrt+abhrt", style="Normal") %>%
    body_add_par("chrt_um = (athrt+abhrt)/vhrt", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Kidney", style="Normal") %>%
    body_add_par("ctkdn = atkdn/(kdnvtbc*vkdn)       # Kidney Tissue Concentration", style="Normal") %>%
    body_add_par("cbkdn = abkdn/((1-kdnvtbc)*vkdn)   # Kidney Blood Concentration", style="Normal") %>%
    body_add_par("akdn = atkdn+abkdn", style="Normal") %>%
    body_add_par("ckdn_um = (atkdn+abkdn)/vkdn", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("GI", style="Normal") %>%
    body_add_par("ctgi = atgi/(givtbc*vgi)       # GI Tissue Concentration", style="Normal") %>%
    body_add_par("cbgi = abgi/((1-givtbc)*vgi)   # GI Blood Concentration", style="Normal") %>%
    body_add_par("agi = atgi+abgi", style="Normal") %>%
    body_add_par("cgi_um = (atgi+abgi)/vgi", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Liver", style="Normal") %>%
    body_add_par("ctliv = atliv/(livvtbc*vliv)       # Liver Tissue Concentration", style="Normal") %>%
    body_add_par("cbliv = abliv/((1-livvtbc)*vliv)  # Liver Blood Concentration", style="Normal") %>%
    body_add_par("aliv = atliv+abliv", style="Normal") %>%
    body_add_par("cliv_um = (atliv+abliv)/vliv", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Rapidly Perfused Tissue", style="Normal") %>%
    body_add_par("ctrpf = atrpf/(rpfvtbc*vrpf)       # Rapidly Perfused Tissue Tissue Concentration", style="Normal") %>%
    body_add_par("cbrpf = abrpf/((1-rpfvtbc)*vrpf)   # Rapidly Perfused Tissue Blood Concentration", style="Normal") %>%
    body_add_par("arpf = atrpf+abrpf", style="Normal") %>%
    body_add_par("crpf_um = (atrpf+abrpf)/vrpf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Slowly Perfused Tissue", style="Normal") %>%
    body_add_par("ctspf = atspf/(spfvtbc*vspf)       # Slowly Perfused Tissue Tissue Concentration", style="Normal") %>%
    body_add_par("cbspf = abspf/((1-spfvtbc)*vspf)   # Slowly Perfused Tissue Blood Concentration", style="Normal") %>%
    body_add_par("aspf = atspf+abspf", style="Normal") %>%
    body_add_par("cspf_um = (atspf+abspf)/vspf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Blood plasma(venous)", style="Normal") %>%
    body_add_par("cv = (qfat*cbfat+qskin*cbskin+qmusc*cbmusc+qbone*cbbone+qbrn*cbbrn+qlng*cblng+qhrt*cbhrt+qkdn*cbkdn+qvliv*cbliv+qrpf*cbrpf+qspf*cbspf)/qcp", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### EXPOSURE CALCULATIONS ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Oral and Drinking Water Exposure", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("rate of oral and drinking water reductions", style="Normal") %>%
    body_add_par("dt(odose) = -ka*odose", style="Normal") %>%
    body_add_par("dt(ddose) = -ka*ddose", style="Normal") %>%
    body_add_par("track tot dose as a state variable to it can plotted effectivly at the end", style="Normal") %>%
    body_add_par("dt(totodose) = 0", style="Normal") %>%
    body_add_par("dt(totddose) = 0", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("dose available in gut lumen at each time point", style="Normal") %>%
    body_add_par("dose_in_gut = ka*(odose+ddose)", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("change in amount of dose absorbed", style="Normal") %>%
    body_add_par("available_dose = dose_in_gut    # chemical available for absorbtion ", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("Oral Vehicle exposure", style="Normal") %>%
    body_add_par("dt(totodosev)=0", style="Normal") %>%
    body_add_par("dt(odosev)=-kfec*odosev-kVtoL*odosev", style="Normal") %>%
    body_add_par("dt(alas) = kVtoL*odosev-kfec*alas-kent*alas", style="Normal") %>%
    body_add_par("dt(akent)= kent*alas", style="Normal") %>%
    body_add_par("dt(afec)= kfec*alas", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("raabsgut = ka*alas + fa*available_dose                     # amount absorbed per time step", style="Normal") %>%
    body_add_par("dt(aabsgut) = raabsgut", style="Normal") %>%
    body_add_par("ranabsgut = (1-fa)*available_dose                   # amount not absorbed per time step", style="Normal") %>%
    body_add_par("dt(anabsgut) = ranabsgut", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("Inhalation Exposure", style="Normal") %>%
    body_add_par("dt(inhswch) = 0", style="Normal") %>%
    body_add_par("rinh = inhswch*qalv*cinh                           #amount inhaled per time in lung tissue compartment", style="Normal") %>%
    body_add_par("dt(ainh) = rinh", style="Normal") %>%
    body_add_par("rexh = inhswch*qalv*cpls/pair                     #amount exhaled per time from lung tissue compartment", style="Normal") %>%
    body_add_par("dt(aexh) = rexh", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Inravenous Exposure", style="Normal") %>%
    body_add_par("dt(ivswch) = 0", style="Normal") %>%
    body_add_par("riv = ivswch*ivdose								 #IV dose per time", style="Normal") %>%
    body_add_par("dt(aiv) =riv", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### RATE OF CHANGE CALCULATIONS ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("fat", style="Normal") %>%
    body_add_par("dt(abfat) = qfat*cpls-qfat*cbfat + pafat*ctfat/pfat - pafat*cbfat", style="Normal") %>%
    body_add_par("dt(atfat) = pafat*cbfat - pafat*ctfat/pfat", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("skin", style="Normal") %>%
    body_add_par("dt(abskin) = qskin*cpls- qskin*cbskin + paskin*ctskin/pskin - paskin*cbskin", style="Normal") %>%
    body_add_par("dt(atskin) = paskin*cbskin - paskin*ctskin/pskin", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("muscle", style="Normal") %>%
    body_add_par("dt(abmusc) = qmusc*cpls- qmusc*cbmusc + pamusc*ctmusc/pmusc - pamusc*cbmusc", style="Normal") %>%
    body_add_par("dt(atmusc) = pamusc*cbmusc - pamusc*ctmusc/pmusc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("bone", style="Normal") %>%
    body_add_par("dt(abbone) = qbone*cpls- qbone*cbbone + pabone*ctbone/pbone - pabone*cbbone", style="Normal") %>%
    body_add_par("dt(atbone) = pabone*cbbone - pabone*ctbone/pbone", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("brain", style="Normal") %>%
    body_add_par("dt(abbrn) = qbrn*cpls- qbrn*cbbrn + pabrn*ctbrn/pbrn - pabrn*cbbrn", style="Normal") %>%
    body_add_par("dt(atbrn) = pabrn*cbbrn - pabrn*ctbrn/pbrn", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("lung", style="Normal") %>%
    body_add_par("dt(ablng) = qlng*cpls- qlng*cblng + palng*ctlng/plng - palng*cblng + rinh - rexh", style="Normal") %>%
    body_add_par("dt(atlng) = palng*cblng - palng*ctlng/plng", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("heart", style="Normal") %>%
    body_add_par("dt(abhrt) = qhrt*cpls- qhrt*cbhrt + pahrt*cthrt/phrt - pahrt*cbhrt", style="Normal") %>%
    body_add_par("dt(athrt) = pahrt*cbhrt - pahrt*cthrt/phrt", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("kidney", style="Normal") %>%
    body_add_par("rauexc = gfr*frwsol*(1-res)*ctkdn/pkdn                          # rate of change for amount of chem urinary excretion", style="Normal") %>%
    body_add_par("dt(auexc) = rauexc", style="Normal") %>%
    body_add_par("dt(abkdn) = qkdn*cpls- qkdn*cbkdn + pakdn*ctkdn/pkdn - pakdn*cbkdn", style="Normal") %>%
    body_add_par("dt(atkdn) = pakdn*cbkdn - pakdn*ctkdn/pkdn - rauexc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("GI", style="Normal") %>%
    body_add_par("dt(abgi) = qgi*cpls - qgi*cbgi + pagi*ctgi/pgi - pagi*cbgi+ raabsgut", style="Normal") %>%
    body_add_par("dt(atgi) = pagi*cbgi - pagi*ctgi/pgi", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Liver", style="Normal") %>%
    body_add_par("rametliv1 = vkm1 * (ctliv*fupls/pliv)                              # amount metabolism in the liver linear", style="Normal") %>%
    body_add_par("dt(ametliv1) = rametliv1", style="Normal") %>%
    body_add_par("rametliv2 = vmaxliv*(ctliv*fupls /pliv)/((ctliv /pliv) + km)      # amount metabolised in the liver saturable", style="Normal") %>%
    body_add_par("dt(ametliv2) = rametliv2", style="Normal") %>%
    body_add_par("dt(abliv) = qaliv*cpls - qvliv*cbliv + qgi*cbgi + paliv*ctliv/pliv - paliv*cbliv", style="Normal") %>%
    body_add_par("dt(atliv) = paliv*cbliv- paliv*ctliv/pliv - rametliv1 - rametliv2", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Rapidly Perfused Tissue", style="Normal") %>%
    body_add_par("dt(abrpf) = qrpf*cpls- qrpf*cbrpf + parpf*ctrpf/prpf - parpf*cbrpf", style="Normal") %>%
    body_add_par("dt(atrpf) = parpf*cbrpf - parpf*ctrpf/prpf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Slowly Perfused Tissue", style="Normal") %>%
    body_add_par("dt(abspf) = qspf*cpls- qspf*cbspf + paspf*ctspf/pspf - paspf*cbspf", style="Normal") %>%
    body_add_par("dt(atspf) = paspf*cbspf - paspf*ctspf/pspf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Blood", style="Normal") %>%
    body_add_par("raclbld = kbld*cpls*fupls                                      # generic first order clearence in blood", style="Normal") %>%
    body_add_par("dt(aclbld) = raclbld", style="Normal") %>%
    body_add_par("dt(abld) = qcp*(cv-cpls) + riv - raclbld", style="Normal") %>%
    body_add_par("dt(aucpls)=cpls", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### MASS BALANCE ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("dose = totodose + totddose + ainh + aiv +totodosev", style="Normal") %>%
    body_add_par("body = abld + afat + askin + amusc + abone + abrn + alng + ahrt + agi + aliv + akdn + arpf + aspf  +odose +ddose+odosev+ alas", style="Normal") %>%
    body_add_par("cleared = ametliv1+ametliv2 + aclbld + auexc + anabsgut + aexh + akent + afec", style="Normal") %>%
    body_add_par("tmass = dose - body - cleared", style="Normal")

 return(HESI_doc)
}

#' Pipe to add concentration-timecourse data to the report
#' @description 
#' @param report_doc 
#' @return report_doc
#' @export
createHESIgraphs <- function(report_doc, context){
  PBPKequations <- "" 
  # @JF, i'm guessing based on the above line that you want this ultimately to draw the plots for variables that are included in the pbpk model. for now, this just takes the "selected" plots in the gui
  report_doc %>%
    cursor_reach('^Model Evaluation$') %>%
    cursor_forward() %>%
    body_add_par(value = "Some models were simulated.", pos = "on") %>%
    body_add_par(value = "Key concentration time-series", style = "heading 3") 
    
  for (tissue in unique(context$variable)) {
    
    subset <- context[which(context$variable==tissue),]
    
    plot <- ggplot2::ggplot(subset, aes(x=time, y=value))+ geom_line()
    report_doc %>% body_add_gg(plot)
  }
}