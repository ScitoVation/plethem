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
 PBPKequations <- paste(
" ###################### CONCENTRATION CALCULATIONS ########################### ",
"      #Blood Plasma (arterial)",
"      cpls = abld/vpls",
" ",
" ",
"      # fat",
"      ctfat = atfat/(fatvtbc*vfat)           # fat Tissue Concentration",
"      cbfat = abfat/((1-fatvtbc)*vfat)       # fat Blood Concentration",
"      afat = atfat + abfat",
"      cfat_um = (atfat+abfat)/vfat",
" ",
" ",
"      # Skin",
"      ctskin = atskin/(skinvtbc*vskin)       # Skin Tissue Concentration",
"      cbskin = abskin/((1-skinvtbc)*vskin)   # SKin Blood Concentration",
"      askin = atskin+abskin",
"      cskin_um = (atskin+abskin)/vskin",
" ",
" ",
"      # Muscle",
"      ctmusc = atmusc/(muscvtbc*vmusc)       # Muscle Tissue Concentration",
"      cbmusc = abmusc/((1-muscvtbc)*vmusc)   # Muscle Blood Concentration",
"      amusc = atmusc+abmusc",
"      cmusc_um = (atmusc+abmusc)/vmusc",
" ",
" ",
"      # Bone",
"      ctbone = atbone/(bonevtbc*vbone)       # Bone Tissue Concentration",
"      cbbone = abbone/((1-bonevtbc)*vbone)   # Bone Blood Concentration",
"      abone = atbone+abbone",
"      cbone_um = (atbone+abbone)/vbone",
" ",
" ",
"      # Brain",
"      ctbrn = atbrn/(brnvtbc*vbrn)       # Brain Tissue Concentration",
"      cbbrn = abbrn/((1-brnvtbc)*vbrn)   # Brain Blood Concentration",
"      abrn = atbrn+abbrn",
"      cbrn_um = (atbrn+abbrn)/vbrn",
" ",
" ",
"      # Lung",
"      ctlng = atlng/(lngvtbc*vlng)       # Lung Tissue Concentration",
"      cblng = ablng/((1-lngvtbc)*vlng)   # Lung Blood Concentration",
"      alng = atlng+ablng",
"      clng_um = (atlng+ablng)/vlng",
" ",
" ",
"      # Heart",
"      cthrt = athrt/(hrtvtbc*vhrt)       # Heart Tissue Concentration",
"      cbhrt = abhrt/((1-hrtvtbc)*vhrt)   # Heart Blood Concentration",
"      ahrt = athrt+abhrt",
"      chrt_um = (athrt+abhrt)/vhrt",
" ",
" ",
"      # Kidney",
"      ctkdn = atkdn/(kdnvtbc*vkdn)       # Kidney Tissue Concentration",
"      cbkdn = abkdn/((1-kdnvtbc)*vkdn)   # Kidney Blood Concentration",
"      akdn = atkdn+abkdn",
"      ckdn_um = (atkdn+abkdn)/vkdn",
" ",
" ",
"      # GI",
"      ctgi = atgi/(givtbc*vgi)       # GI Tissue Concentration",
"      cbgi = abgi/((1-givtbc)*vgi)   # GI Blood Concentration",
"      agi = atgi+abgi",
"      cgi_um = (atgi+abgi)/vgi",
" ",
"      # Liver",
"      ctliv = atliv/(livvtbc*vliv)       # Liver Tissue Concentration",
"      cbliv = abliv/((1-livvtbc)*vliv)  # Liver Blood Concentration",
"      aliv = atliv+abliv",
"      cliv_um = (atliv+abliv)/vliv",
" ",
" ",
"      # Rapidly Perfused Tissue",
"      ctrpf = atrpf/(rpfvtbc*vrpf)       # Rapidly Perfused Tissue Tissue Concentration",
"      cbrpf = abrpf/((1-rpfvtbc)*vrpf)   # Rapidly Perfused Tissue Blood Concentration",
"      arpf = atrpf+abrpf",
"      crpf_um = (atrpf+abrpf)/vrpf",
" ",
" ",
"      # Slowly Perfused Tissue",
"      ctspf = atspf/(spfvtbc*vspf)       # Slowly Perfused Tissue Tissue Concentration",
"      cbspf = abspf/((1-spfvtbc)*vspf)   # Slowly Perfused Tissue Blood Concentration",
"      aspf = atspf+abspf",
"      cspf_um = (atspf+abspf)/vspf",
" ",
" ",
"      #Blood plasma(venous)",
"      cv = (qfat*cbfat+qskin*cbskin+qmusc*cbmusc+qbone*cbbone+qbrn*cbbrn+qlng*cblng+qhrt*cbhrt+qkdn*cbkdn+qvliv*cbliv+qrpf*cbrpf+qspf*cbspf)/qcp",
" ",
"      ###################### EXPOSURE CALCULATIONS ##############################",
" ",
"      # Oral and Drinking Water Exposure",
" ",
" ",
"      #rate of oral and drinking water reductions",
"      dt(odose) = -ka*odose",
"      dt(ddose) = -ka*ddose",
"      # track tot dose as a state variable to it can plotted effectivly at the end",
"      dt(totodose) = 0",
"      dt(totddose) = 0",
"       ",
"      # dose available in gut lumen at each time point",
"      dose_in_gut = ka*(odose+ddose)",
" ",
"      #change in amount of dose absorbed",
"      available_dose = dose_in_gut    # chemical available for absorbtion ",
"      ",
"      #Oral Vehicle exposure",
"      dt(totodosev)=0",
"      dt(odosev)=-kfec*odosev-kVtoL*odosev",
"      dt(alas) = kVtoL*odosev-kfec*alas-kent*alas",
"      dt(akent)= kent*alas",
"      dt(afec)= kfec*alas",
"      ",
"      ",
"      raabsgut = ka*alas + fa*available_dose                     # amount absorbed per time step",
"      dt(aabsgut) = raabsgut",
"      ranabsgut = (1-fa)*available_dose                   # amount not absorbed per time step",
"      dt(anabsgut) = ranabsgut",
"      ",
"      ",
"      ",
"      # Inhalation Exposure",
"      dt(inhswch) = 0",
"      rinh = inhswch*qalv*cinh                           #amount inhaled per time in lung tissue compartment",
"      dt(ainh) = rinh",
"      rexh = inhswch*qalv*cpls/pair                     #amount exhaled per time from lung tissue compartment",
"      dt(aexh) = rexh",
" ",
"      # Inravenous Exposure",
"      dt(ivswch) = 0",
"      riv = ivswch*ivdose								 #IV dose per time",
"      dt(aiv) =riv                           ",
" ",
"      ###################### RATE OF CHANGE CALCULATIONS #########################",
" ",
"      # fat",
"      dt(abfat) = qfat*cpls-qfat*cbfat + pafat*ctfat/pfat - pafat*cbfat",
"      dt(atfat) = pafat*cbfat - pafat*ctfat/pfat",
" ",
"      #skin",
"      dt(abskin) = qskin*cpls- qskin*cbskin + paskin*ctskin/pskin - paskin*cbskin",
"      dt(atskin) = paskin*cbskin - paskin*ctskin/pskin",
" ",
"      #muscle",
"      dt(abmusc) = qmusc*cpls- qmusc*cbmusc + pamusc*ctmusc/pmusc - pamusc*cbmusc",
"      dt(atmusc) = pamusc*cbmusc - pamusc*ctmusc/pmusc",
" ",
"      #bone",
"      dt(abbone) = qbone*cpls- qbone*cbbone + pabone*ctbone/pbone - pabone*cbbone",
"      dt(atbone) = pabone*cbbone - pabone*ctbone/pbone",
" ",
"      #brain",
"      dt(abbrn) = qbrn*cpls- qbrn*cbbrn + pabrn*ctbrn/pbrn - pabrn*cbbrn",
"      dt(atbrn) = pabrn*cbbrn - pabrn*ctbrn/pbrn",
" ",
"      #lung",
"      dt(ablng) = qlng*cpls- qlng*cblng + palng*ctlng/plng - palng*cblng + rinh - rexh",
"      dt(atlng) = palng*cblng - palng*ctlng/plng",
" ",
"      #heart",
"      dt(abhrt) = qhrt*cpls- qhrt*cbhrt + pahrt*cthrt/phrt - pahrt*cbhrt",
"      dt(athrt) = pahrt*cbhrt - pahrt*cthrt/phrt",
" ",
"      #kidney",
"      #dt(volurine) = uflw*bw                                              # dtrate of urine generation",
"      rauexc = gfr*frwsol*(1-res)*ctkdn/pkdn                          # rate of change for amount of chem urinary excretion",
"      dt(auexc) = rauexc",
"      dt(abkdn) = qkdn*cpls- qkdn*cbkdn + pakdn*ctkdn/pkdn - pakdn*cbkdn",
"      dt(atkdn) = pakdn*cbkdn - pakdn*ctkdn/pkdn - rauexc",
" ",
"      #GI",
"      dt(abgi) = qgi*cpls - qgi*cbgi + pagi*ctgi/pgi - pagi*cbgi+ raabsgut",
"      dt(atgi) = pagi*cbgi - pagi*ctgi/pgi",
"      #Liver",
"      rametliv1 = vkm1 * (ctliv*fupls/pliv)                              # amount metabolism in the liver linear",
"      dt(ametliv1) = rametliv1",
"      rametliv2 = vmaxliv*(ctliv*fupls /pliv)/((ctliv /pliv) + km)      # amount metabolised in the liver saturable",
"      dt(ametliv2) = rametliv2",
"      dt(abliv) = qaliv*cpls - qvliv*cbliv + qgi*cbgi + paliv*ctliv/pliv - paliv*cbliv",
"      dt(atliv) = paliv*cbliv- paliv*ctliv/pliv - rametliv1 - rametliv2",
" ",
"      #Rapidly Perfused Tissue",
"      dt(abrpf) = qrpf*cpls- qrpf*cbrpf + parpf*ctrpf/prpf - parpf*cbrpf",
"      dt(atrpf) = parpf*cbrpf - parpf*ctrpf/prpf",
" ",
"      #Slowly Perfused Tissue",
"      dt(abspf) = qspf*cpls- qspf*cbspf + paspf*ctspf/pspf - paspf*cbspf",
"      dt(atspf) = paspf*cbspf - paspf*ctspf/pspf",
" ",
" ",
"      #Blood",
"      raclbld = kbld*cpls*fupls                                      # generic first order clearence in blood",
"      dt(aclbld) = raclbld",
"      dt(abld) = qcp*(cv-cpls) + riv - raclbld",
"      dt(aucpls)=cpls",
" ",
" ",
" ",
"      ################################ MASS BALANCE ##############################",
" ",
"      dose = totodose + totddose + ainh + aiv +totodosev",
"      body = abld + afat + askin + amusc + abone + abrn + alng + ahrt + agi + aliv + akdn + arpf + aspf  +odose +ddose+odosev+ alas",
"      cleared = ametliv1+ametliv2 + aclbld + auexc + anabsgut + aexh + akent + afec",
"      tmass = dose - body - cleared",
sep = "\n")
 return(PBPKequations)
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