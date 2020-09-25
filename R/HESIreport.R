# This file contains utility functions for PBPK model reporting
# none of these functions should be called by the user

#' Creates the PBPK model flowchart. Should not be used by directly by the user
#' @description The function returns the PBPK model flowchart
#' @param HESI_doc This is the PBPK modeling document that the flowchart will be added too.
#' @return The PBPK modeling document with the PBPK flowchart added to it.
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



#' Add PBPK equations to the PBPK reporting document. Should not be used by directly by the user
#' @description The function returns the PBPK reporting document with the PBPK modeling equations
#' and starting variable values to it.
#' @param HESI_doc This is the PBPK modeling document that the flowchart will be added too.
#' @return PBPK modeling document with equations and starting parameter values. 
#' @export
addPBPKequations <- function(HESI_doc){
  HESI_doc %>%
    cursor_reach('^Model Equations$') %>%
    cursor_forward() %>%
    body_add_par("### CONCENTRATION CALCULATIONS ###", style="Normal") %>%
    body_add_par("Blood Plasma (arterial)", style="Normal") %>%
    body_add_par("cpls = abld/vpls", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Fat", style="Normal") %>%
    body_add_par("Fat Tissue Concentration", style="Normal") %>%
    body_add_par("ctfat = atfat/(fatvtbc*vfat)", style="Normal") %>%
    body_add_par("Fat Blood Concentration", style="Normal") %>%
    body_add_par("cbfat = abfat/((1-fatvtbc)*vfat)", style="Normal") %>%
    body_add_par("afat = atfat + abfat", style="Normal") %>%
    body_add_par("cfat_um = (atfat+abfat)/vfat", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Skin", style="Normal") %>%
    body_add_par("Skin Tissue Concentration", style="Normal") %>%
    body_add_par("ctskin = atskin/(skinvtbc*vskin)", style="Normal") %>%
    body_add_par("Skin Blood Concentration", style="Normal") %>%
    body_add_par("cbskin = abskin/((1-skinvtbc)*vskin)", style="Normal") %>%
    body_add_par("askin = atskin+abskin", style="Normal") %>%
    body_add_par("cskin_um = (atskin+abskin)/vskin", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Muscle", style="Normal") %>%
    body_add_par("Muscle Tissue Concentration", style="Normal") %>%
    body_add_par("ctmusc = atmusc/(muscvtbc*vmusc)", style="Normal") %>%
    body_add_par("Muscle Blood Concentration", style="Normal") %>%
    body_add_par("cbmusc = abmusc/((1-muscvtbc)*vmusc)", style="Normal") %>%
    body_add_par("amusc = atmusc+abmusc", style="Normal") %>%
    body_add_par("cmusc_um = (atmusc+abmusc)/vmusc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Bone", style="Normal") %>%
    body_add_par("Bone Tissue Concentration", style="Normal") %>%
    body_add_par("ctbone = atbone/(bonevtbc*vbone)", style="Normal") %>%
    body_add_par("Bone Blood Concentration", style="Normal") %>%
    body_add_par("cbbone = abbone/((1-bonevtbc)*vbone)", style="Normal") %>%
    body_add_par("abone = atbone+abbone", style="Normal") %>%
    body_add_par("cbone_um = (atbone+abbone)/vbone", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Brain", style="Normal") %>%
    body_add_par("Brain Tissue Concentration", style="Normal") %>%
    body_add_par("ctbrn = atbrn/(brnvtbc*vbrn)", style="Normal") %>%
    body_add_par("Brain Blood Concentration", style="Normal") %>%
    body_add_par("cbbrn = abbrn/((1-brnvtbc)*vbrn)", style="Normal") %>%
    body_add_par("abrn = atbrn+abbrn", style="Normal") %>%
    body_add_par("cbrn_um = (atbrn+abbrn)/vbrn", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Lung", style="Normal") %>%
    body_add_par("Lung Tissue Concentration", style="Normal") %>%
    body_add_par("ctlng = atlng/(lngvtbc*vlng)", style="Normal") %>%
    body_add_par("Lung Blood Concentration", style="Normal") %>%
    body_add_par("cblng = ablng/((1-lngvtbc)*vlng)", style="Normal") %>%
    body_add_par("alng = atlng+ablng", style="Normal") %>%
    body_add_par("clng_um = (atlng+ablng)/vlng", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Heart", style="Normal") %>%
    body_add_par("Heart Tissue Concentration", style="Normal") %>%
    body_add_par("cthrt = athrt/(hrtvtbc*vhrt)", style="Normal") %>%
    body_add_par("Heart Blood Concentration", style="Normal") %>%
    body_add_par("cbhrt = abhrt/((1-hrtvtbc)*vhrt)", style="Normal") %>%
    body_add_par("ahrt = athrt+abhrt", style="Normal") %>%
    body_add_par("chrt_um = (athrt+abhrt)/vhrt", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Kidney", style="Normal") %>%
    body_add_par("Kidney Tissue Concentration", style="Normal") %>%
    body_add_par("ctkdn = atkdn/(kdnvtbc*vkdn)", style="Normal") %>%
    body_add_par("Kidney Blood Concentration", style="Normal") %>%
    body_add_par("cbkdn = abkdn/((1-kdnvtbc)*vkdn)", style="Normal") %>%
    body_add_par("akdn = atkdn+abkdn", style="Normal") %>%
    body_add_par("ckdn_um = (atkdn+abkdn)/vkdn", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("GI", style="Normal") %>%
    body_add_par("GI Tissue Concentration", style="Normal") %>%
    body_add_par("ctgi = atgi/(givtbc*vgi)", style="Normal") %>%
    body_add_par("GI Blood Concentration", style="Normal") %>%
    body_add_par("cbgi = abgi/((1-givtbc)*vgi)", style="Normal") %>%
    body_add_par("agi = atgi+abgi", style="Normal") %>%
    body_add_par("cgi_um = (atgi+abgi)/vgi", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Liver", style="Normal") %>%
    body_add_par("Liver Tissue Concentration", style="Normal") %>%
    body_add_par("ctliv = atliv/(livvtbc*vliv)", style="Normal") %>%
    body_add_par("Liver Blood Concentration", style="Normal") %>%
    body_add_par("cbliv = abliv/((1-livvtbc)*vliv)", style="Normal") %>%
    body_add_par("aliv = atliv+abliv", style="Normal") %>%
    body_add_par("cliv_um = (atliv+abliv)/vliv", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Rapidly Perfused Tissue", style="Normal") %>%
    body_add_par("Rapidly Perfused Tissue Tissue Concentration", style="Normal") %>%
    body_add_par("ctrpf = atrpf/(rpfvtbc*vrpf)", style="Normal") %>%
    body_add_par("Rapidly Perfused Tissue Blood Concentration", style="Normal") %>%
    body_add_par("cbrpf = abrpf/((1-rpfvtbc)*vrpf)", style="Normal") %>%
    body_add_par("arpf = atrpf+abrpf", style="Normal") %>%
    body_add_par("crpf_um = (atrpf+abrpf)/vrpf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Slowly Perfused Tissue", style="Normal") %>%
    body_add_par("Slowly Perfused Tissue Tissue Concentration", style="Normal") %>%
    body_add_par("ctspf = atspf/(spfvtbc*vspf)", style="Normal") %>%
    body_add_par("Slowly Perfused Tissue Blood Concentration", style="Normal") %>%
    body_add_par("cbspf = abspf/((1-spfvtbc)*vspf)", style="Normal") %>%
    body_add_par("aspf = atspf+abspf", style="Normal") %>%
    body_add_par("cspf_um = (atspf+abspf)/vspf", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Blood plasma(venous)", style="Normal") %>%
    body_add_par("cv = (qfat * cbfat + qskin * cbskin + qmusc * cbmusc + qbone * cbbone + qbrn * cbbrn + qlng * cblng + qhrt * cbhrt + qkdn * cbkdn + qvliv * cbliv + qrpf * cbrpf + qspf * cbspf)/qcp", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### EXPOSURE CALCULATIONS ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Oral and Drinking Water Exposure", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("rate of oral and drinking water reductions", style="Normal") %>%
    body_add_par("dt(odose) = -ka*odose", style="Normal") %>%
    body_add_par("dt(ddose) = -ka*ddose", style="Normal") %>%
    body_add_par("dt(totodose) = 0", style="Normal") %>%
    body_add_par("dt(totddose) = 0", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("dose available in gut lumen at each time point", style="Normal") %>%
    body_add_par("dose_in_gut = ka*(odose+ddose)", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Change in Amount of Dose Absorbed", style="Normal") %>%
    body_add_par("Chemical Available for Absorbtion ", style="Normal") %>%
    body_add_par("available_dose = dose_in_gut", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("Oral Vehicle Exposure", style="Normal") %>%
    body_add_par("dt(totodosev)=0", style="Normal") %>%
    body_add_par("dt(odosev)=-kfec*odosev-kVtoL*odosev", style="Normal") %>%
    body_add_par("dt(alas) = kVtoL*odosev-kfec*alas-kent*alas", style="Normal") %>%
    body_add_par("dt(akent)= kent*alas", style="Normal") %>%
    body_add_par("dt(afec)= kfec*alas", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("Amount Absorbed per Time Step", style="Normal") %>%
    body_add_par("raabsgut = ka*alas + fa*available_dose", style="Normal") %>%
    body_add_par("dt(aabsgut) = raabsgut", style="Normal") %>%
    body_add_par("Amount Not Absorbed per Time Step", style="Normal") %>%
    body_add_par("ranabsgut = (1-fa)*available_dose", style="Normal") %>%
    body_add_par("dt(anabsgut) = ranabsgut", style="Normal") %>%
    body_add_par("      ", style="Normal") %>%
    body_add_par("Inhalation Exposure", style="Normal") %>%
    body_add_par("dt(inhswch) = 0", style="Normal") %>%
    body_add_par("Amount Inhaled per Time in Lung Tissue Compartment", style="Normal") %>%
    body_add_par("rinh = inhswch*qalv*cinh", style="Normal") %>%
    body_add_par("dt(ainh) = rinh", style="Normal") %>%
    body_add_par("Amount Exhaled per Time From Lung Tissue Compartment", style="Normal") %>%
    body_add_par("rexh = inhswch*qalv*cpls/pair", style="Normal") %>%
    body_add_par("dt(aexh) = rexh", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Inravenous Exposure", style="Normal") %>%
    body_add_par("dt(ivswch) = 0", style="Normal") %>%
    body_add_par("IV dose per time", style="Normal") %>%
    body_add_par("riv = ivswch*ivdose", style="Normal") %>%
    body_add_par("dt(aiv) =riv", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### RATE OF CHANGE CALCULATIONS ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Fat", style="Normal") %>%
    body_add_par("dt(abfat) = qfat*cpls-qfat*cbfat + pafat*ctfat/pfat - pafat*cbfat", style="Normal") %>%
    body_add_par("dt(atfat) = pafat*cbfat - pafat*ctfat/pfat", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Skin", style="Normal") %>%
    body_add_par("dt(abskin) = qskin*cpls- qskin*cbskin + paskin*ctskin/pskin - paskin*cbskin", style="Normal") %>%
    body_add_par("dt(atskin) = paskin*cbskin - paskin*ctskin/pskin", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Muscle", style="Normal") %>%
    body_add_par("dt(abmusc) = qmusc*cpls- qmusc*cbmusc + pamusc*ctmusc/pmusc - pamusc*cbmusc", style="Normal") %>%
    body_add_par("dt(atmusc) = pamusc*cbmusc - pamusc*ctmusc/pmusc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Bone", style="Normal") %>%
    body_add_par("dt(abbone) = qbone*cpls- qbone*cbbone + pabone*ctbone/pbone - pabone*cbbone", style="Normal") %>%
    body_add_par("dt(atbone) = pabone*cbbone - pabone*ctbone/pbone", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Brain", style="Normal") %>%
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
    body_add_par("Rate of Change for Amount of Chem Urinary Excretion", style="Normal") %>%
    body_add_par("rauexc = gfr*frwsol*(1-res)*ctkdn/pkdn", style="Normal") %>%
    body_add_par("dt(auexc) = rauexc", style="Normal") %>%
    body_add_par("dt(abkdn) = qkdn*cpls- qkdn*cbkdn + pakdn*ctkdn/pkdn - pakdn*cbkdn", style="Normal") %>%
    body_add_par("dt(atkdn) = pakdn*cbkdn - pakdn*ctkdn/pkdn - rauexc", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("GI", style="Normal") %>%
    body_add_par("dt(abgi) = qgi*cpls - qgi*cbgi + pagi*ctgi/pgi - pagi*cbgi+ raabsgut", style="Normal") %>%
    body_add_par("dt(atgi) = pagi*cbgi - pagi*ctgi/pgi", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("Liver", style="Normal") %>%
    body_add_par("Amount Metabolism in The Liver Linear", style="Normal") %>%
    body_add_par("rametliv1 = vkm1 * (ctliv*fupls/pliv)", style="Normal") %>%
    body_add_par("dt(ametliv1) = rametliv1", style="Normal") %>%
    body_add_par("Amount Metabolised in The Liver Saturable", style="Normal") %>%
    body_add_par("rametliv2 = vmaxliv*(ctliv*fupls /pliv)/((ctliv /pliv) + km)", style="Normal") %>%
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
    body_add_par("Generic First Order Clearence in Blood", style="Normal") %>%
    body_add_par("raclbld = kbld*cpls*fupls", style="Normal") %>%
    body_add_par("dt(aclbld) = raclbld", style="Normal") %>%
    body_add_par("dt(abld) = qcp*(cv-cpls) + riv - raclbld", style="Normal") %>%
    body_add_par("dt(aucpls)=cpls", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par(" ### MASS BALANCE ### ", style="Normal") %>%
    body_add_par(" ", style="Normal") %>%
    body_add_par("dose = totodose + totddose + ainh + aiv + totodosev", style="Normal") %>%
    body_add_par("body = abld + afat + askin + amusc + abone + abrn + alng + ahrt + agi + aliv + akdn + arpf + aspf + odose + ddose + odosev + alas", style="Normal") %>%
    body_add_par("cleared = ametliv1 + ametliv2 + aclbld + auexc + anabsgut + aexh + akent + afec", style="Normal") %>%
    body_add_par("tmass = dose - body - cleared", style="Normal")
  
  HESI_doc %>%
    cursor_reach('^Model Parameters$') %>%
    cursor_forward() %>%
    body_add_par(" Initial Parameters ", style = "Normal") %>%
    body_add_par("mw = 0", style="Normal") %>%
    body_add_par("bdose = 0", style="Normal") %>%
    body_add_par("blen = 0", style="Normal") %>%
    body_add_par("breps = 0", style="Normal") %>%
    body_add_par("totbreps = 0", style="Normal") %>%
    body_add_par("drdose = 0", style="Normal") %>%
    body_add_par("vdw = 0", style="Normal") %>%
    body_add_par("dreps = 0", style="Normal") %>%
    body_add_par("inhdose = 0", style="Normal") %>%
    body_add_par("inhtlen = 0", style="Normal") %>%
    body_add_par("inhdays = 0", style="Normal") %>%
    body_add_par("ivdose = 0", style="Normal") %>%
    body_add_par("ivlen = 0", style="Normal") %>%
    body_add_par("dermrate = 0", style="Normal") %>%
    body_add_par("KPtot = 0", style="Normal") %>%
    body_add_par("Kevap = 0", style="Normal") %>%
    body_add_par("maxcap = 0", style="Normal") %>%
    body_add_par("wsol = 0", style="Normal") %>%
    body_add_par("skarea = 0", style="Normal") %>%
    body_add_par("bdosev = 0", style="Normal") %>%
    body_add_par("blenv = 0", style="Normal") %>%
    body_add_par("brepsv = 0", style="Normal") %>%
    body_add_par("totbrepsv = 0", style="Normal") %>%
    body_add_par("kfec = 0", style="Normal") %>%
    body_add_par("kVtoL = 0", style="Normal") %>%
    body_add_par("kent = 0", style="Normal") %>%
    body_add_par("bw = 0", style="Normal") %>%
    body_add_par("qcc = 0", style="Normal") %>%
    body_add_par("hct = 0", style="Normal") %>%
    body_add_par("vbldc = 0", style="Normal") %>%
    body_add_par("perfc = 0", style="Normal") %>%
    body_add_par("kbld = 0", style="Normal") %>%
    body_add_par("respr = 0", style="Normal") %>%
    body_add_par("tv = 0", style="Normal") %>%
    body_add_par("ds = 0", style="Normal") %>%
    body_add_par("uflw = 0", style="Normal") %>%
    body_add_par("gfr = 0", style="Normal") %>%
    body_add_par("frwsol = 0", style="Normal") %>%
    body_add_par("fatvtbc = 0", style="Normal") %>%
    body_add_par("vfatc = 0", style="Normal") %>%
    body_add_par("qfatc = 0", style="Normal") %>%
    body_add_par("pfat = 0", style="Normal") %>%
    body_add_par("skinvtbc = 0", style="Normal") %>%
    body_add_par("vskinc = 0", style="Normal") %>%
    body_add_par("qskinc = 0", style="Normal") %>%
    body_add_par("pskin = 0", style="Normal") %>%
    body_add_par("muscvtbc = 0", style="Normal") %>%
    body_add_par("vmuscc = 0", style="Normal") %>%
    body_add_par("qmuscc = 0", style="Normal") %>%
    body_add_par("pmusc = 0", style="Normal") %>%
    body_add_par("bonevtbc = 0", style="Normal") %>%
    body_add_par("vbonec = 0", style="Normal") %>%
    body_add_par("qbonec = 0", style="Normal") %>%
    body_add_par("pbone = 0", style="Normal") %>%
    body_add_par("brnvtbc = 0", style="Normal") %>%
    body_add_par("vbrnc = 0", style="Normal") %>%
    body_add_par("qbrnc = 0", style="Normal") %>%
    body_add_par("pbrn = 0", style="Normal") %>%
    body_add_par("lngvtbc = 0", style="Normal") %>%
    body_add_par("vlngc = 0", style="Normal") %>%
    body_add_par("qlngc = 0", style="Normal") %>%
    body_add_par("plng = 0", style="Normal") %>%
    body_add_par("hrtvtbc = 0", style="Normal") %>%
    body_add_par("vhrtc = 0", style="Normal") %>%
    body_add_par("qhrtc = 0", style="Normal") %>%
    body_add_par("phrt = 0", style="Normal") %>%
    body_add_par("givtbc = 0", style="Normal") %>%
    body_add_par("vgic = 0", style="Normal") %>%
    body_add_par("qgic = 0", style="Normal") %>%
    body_add_par("pgi = 0", style="Normal") %>%
    body_add_par("fa = 0", style="Normal") %>%
    body_add_par("ka = 0", style="Normal") %>%
    body_add_par("livvtbc = 0", style="Normal") %>%
    body_add_par("vlivc = 0", style="Normal") %>%
    body_add_par("qalivc = 0", style="Normal") %>%
    body_add_par("qvlivc = 0", style="Normal") %>%
    body_add_par("pliv = 0", style="Normal") %>%
    body_add_par("kdnvtbc = 0", style="Normal") %>%
    body_add_par("vkdnc = 0", style="Normal") %>%
    body_add_par("qkdnc = 0", style="Normal") %>%
    body_add_par("pkdn = 0", style="Normal") %>%
    body_add_par("rpfvtbc = 0", style="Normal") %>%
    body_add_par("vrpfc = 0", style="Normal") %>%
    body_add_par("qrpfc = 0", style="Normal") %>%
    body_add_par("prpf = 0", style="Normal") %>%
    body_add_par("spfvtbc = 0", style="Normal") %>%
    body_add_par("vspfc = 0", style="Normal") %>%
    body_add_par("qspfc = 0", style="Normal") %>%
    body_add_par("pspf = 0", style="Normal") %>%
    body_add_par("res = 0", style="Normal") %>%
    body_add_par("fupls = 0", style="Normal") %>%
    body_add_par("vbld = 0", style="Normal") %>%
    body_add_par("vpls = 0", style="Normal") %>%
    body_add_par("vfat = 0", style="Normal") %>%
    body_add_par("vskin = 0", style="Normal") %>%
    body_add_par("vmusc = 0", style="Normal") %>%
    body_add_par("vbone = 0", style="Normal") %>%
    body_add_par("vbrn = 0", style="Normal") %>%
    body_add_par("vlng = 0", style="Normal") %>%
    body_add_par("vhrt = 0", style="Normal") %>%
    body_add_par("vkdn = 0", style="Normal") %>%
    body_add_par("vgi = 0", style="Normal") %>%
    body_add_par("vliv = 0", style="Normal") %>%
    body_add_par("vrpf = 0", style="Normal") %>%
    body_add_par("vspf = 0", style="Normal") %>%
    body_add_par("total_perf = 0", style="Normal") %>%
    body_add_par("qcp = 0", style="Normal") %>%
    body_add_par("qfat = 0", style="Normal") %>%
    body_add_par("qskin = 0", style="Normal") %>%
    body_add_par("qmusc = 0", style="Normal") %>%
    body_add_par("qbone = 0", style="Normal") %>%
    body_add_par("qbrn = 0", style="Normal") %>%
    body_add_par("qlng = 0", style="Normal") %>%
    body_add_par("qhrt = 0", style="Normal") %>%
    body_add_par("qkdn = 0", style="Normal") %>%
    body_add_par("qvliv = 0", style="Normal") %>%
    body_add_par("qgi = 0", style="Normal") %>%
    body_add_par("qaliv = 0", style="Normal") %>%
    body_add_par("qrpf = 0", style="Normal") %>%
    body_add_par("qspf = 0", style="Normal") %>%
    body_add_par("pafat = 0", style="Normal") %>%
    body_add_par("paskin = 0", style="Normal") %>%
    body_add_par("pamusc = 0", style="Normal") %>%
    body_add_par("pabone = 0", style="Normal") %>%
    body_add_par("pabrn = 0", style="Normal") %>%
    body_add_par("palng = 0", style="Normal") %>%
    body_add_par("pahrt = 0", style="Normal") %>%
    body_add_par("pakdn = 0", style="Normal") %>%
    body_add_par("pagi = 0", style="Normal") %>%
    body_add_par("paliv = 0", style="Normal") %>%
    body_add_par("parpf = 0", style="Normal") %>%
    body_add_par("paspf = 0", style="Normal") %>%
    body_add_par("vkm1 = 0", style="Normal") %>%
    body_add_par("vmaxliv = 0", style="Normal") %>%
    body_add_par("km = 0", style="Normal") %>%
    body_add_par("cinh = 0", style="Normal") %>%
    body_add_par("qalv = 0", style="Normal") %>%
    body_add_par("pair = 1e10", style="Normal") %>%
    body_add_par("fuplsmet = 1", style="Normal") %>%
    body_add_par("vdmet = 1e-10", style="Normal") %>%
    body_add_par(" Initial States ", style = "Normal") %>%
    body_add_par("inhswch = 0.0", style="Normal") %>%
    body_add_par("ainh = 0.0", style="Normal") %>%
    body_add_par("aexh = 0.0", style="Normal") %>%
    body_add_par("totodose = 0.0", style="Normal") %>%
    body_add_par("odose = 0.0", style="Normal") %>%
    body_add_par("totddose = 0.0", style="Normal") %>%
    body_add_par("ddose = 0.0", style="Normal") %>%
    body_add_par("odosev = 0.0", style="Normal") %>%
    body_add_par("totodosev = 0.0", style="Normal") %>%
    body_add_par("alas = 0.0", style="Normal") %>%
    body_add_par("akent = 0.0", style="Normal") %>%
    body_add_par("afec = 0.0", style="Normal") %>%
    body_add_par("aabsgut = 0.0", style="Normal") %>%
    body_add_par("ivswch = 0.0", style="Normal") %>%
    body_add_par("aiv = 0.0", style="Normal") %>%
    body_add_par("dermswch = 0.0", style="Normal") %>%
    body_add_par("aderm = 0.0", style="Normal") %>%
    body_add_par("adermabs = 0.0", style="Normal") %>%
    body_add_par("adermevap = 0.0", style="Normal") %>%
    body_add_par("abld = 0.0", style="Normal") %>%
    body_add_par("abfat = 0.0", style="Normal") %>%
    body_add_par("atfat = 0.0", style="Normal") %>%
    body_add_par("abskin = 0.0", style="Normal") %>%
    body_add_par("asc = 0.0", style="Normal") %>%
    body_add_par("ascMgcm2 = 0.0", style="Normal") %>%
    body_add_par("atskin = 0.0", style="Normal") %>%
    body_add_par("abmusc = 0.0", style="Normal") %>%
    body_add_par("atmusc = 0.0", style="Normal") %>%
    body_add_par("abbone = 0.0", style="Normal") %>%
    body_add_par("atbone = 0.0", style="Normal") %>%
    body_add_par("abbrn = 0.0", style="Normal") %>%
    body_add_par("atbrn = 0.0", style="Normal") %>%
    body_add_par("ablng = 0.0", style="Normal") %>%
    body_add_par("atlng = 0.0", style="Normal") %>%
    body_add_par("abhrt = 0.0", style="Normal") %>%
    body_add_par("athrt = 0.0", style="Normal") %>%
    body_add_par("abgi = 0.0", style="Normal") %>%
    body_add_par("atgi = 0.0", style="Normal") %>%
    body_add_par("abliv = 0.0", style="Normal") %>%
    body_add_par("atliv = 0.0", style="Normal") %>%
    body_add_par("abkdn = 0.0", style="Normal") %>%
    body_add_par("atkdn = 0.0", style="Normal") %>%
    body_add_par("abrpf = 0.0", style="Normal") %>%
    body_add_par("atrpf = 0.0", style="Normal") %>%
    body_add_par("abspf = 0.0", style="Normal") %>%
    body_add_par("atspf = 0.0", style="Normal") %>%
    body_add_par("ametliv1 = 0.0", style="Normal") %>%
    body_add_par("ametliv2 = 0.0", style="Normal") %>%
    body_add_par("aclbld = 0.0", style="Normal") %>%
    body_add_par("auexc = 0.0", style="Normal") %>%
    body_add_par("anabsgut = 0.0", style="Normal") %>%
    body_add_par("auexcmet = 0.0", style="Normal") %>%
    body_add_par("amet = 0.0", style="Normal") %>%
    body_add_par("vurine = 1e-10", style="Normal")

 return(HESI_doc)
}



#' Add user selected parameters to the PBPK reporting document.
#' Should not be used by directly by the user
#' @description add user defined parameter tables
#' @param HESI_doc This is the PBPK modeling document that the flowchart will be added too.
#' @param chemical dataframe of chemical data added by the user
#' @param exposure dataframe of exposure data added by the user
#' @param physiological dataframe of physiological data added by the user
#' @return HESI_doc, the same officer::read_docx object 
#' @export
addParameters <- function(HESI_doc, chemical, exposure, physiological){
  HESI_doc %>%
    cursor_reach('^Model Simulations$') %>%
    cursor_forward() %>%
    body_add_par("Chemical User Set Parameters", style="Normal") %>%
    body_add_table(chemical) %>%
    body_add_par("Exposure User Set Parameters", style="Normal") %>%
    body_add_table(exposure) %>%
    body_add_par("Physiological User Set Parameters", style="Normal") %>%
    body_add_table(physiological)
  HESI_doc %>%
    cursor_reach('^Software$') %>%
    cursor_forward() %>%
    body_add_par("PLETHEM version 1.1.0")
    return(HESI_doc)
}

#' Pipe to add concentration-timecourse data to the report
#' Should not be used by directly by the user
#' @description add graphs to the PBPK reporting document
#' @param report_doc an officer::read_docx object 
#' @param context a data.frame of concentrationXtime data, as returned by concData()
#' @param conc_units a string denoting the units of concentration
#' @return report_doc, the same officer::read_docx object 
#' @export
createHESIgraphs <- function(report_doc, context, conc_units){
  PBPKequations <- "" 
  # @JF, i'm guessing based on the above line that you want this ultimately to draw the plots for variables that are included in the pbpk model. for now, this just takes the "selected" plots in the gui
  report_doc %>%
    cursor_reach('^Model Evaluation$') %>%
    cursor_forward() %>%
    body_add_par(value = "Some models were simulated.", pos = "on") %>%
    body_add_par(value = "Key concentration time-series", style = "heading 3") 
  
  for (tissue in unique(context$variable)) {
    
    subset <- context[which(context$variable==tissue),]
    
    plot <- ggplot2::ggplot(subset, aes(x=time, y=value)) +
      geom_line() +
      labs(
        x = 'Time (h)', #FIXME is this always 'hours'?
        y = paste0(tissue, ' concentration (', conc_units ,')')
      )
    
    report_doc %>% body_add_gg(plot)
  }
}
