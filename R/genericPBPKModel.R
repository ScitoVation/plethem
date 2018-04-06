genericPBPKModel <- function(t,state,params){
  with(
    as.list(c(state,params)),{

      ###################### CONCENTRATION CALCULATIONS ###########################

      #Blood Plasma (arterial)
      cpls <- abld/vpls


      # fat
      ctfat <- atfat/(fatvtbc*vfat)           # fat Tissue Concentration
      cbfat <- abfat/((1-fatvtbc)*vfat)       # fat Blood Concentration
      afat <- atfat + abfat
      cfat_um <- (atfat+abfat)/vfat
      cfat_mg <- (atfat+abfat)*mw/vfat

      # Skin
      ctskin <- atskin/(skinvtbc*vskin)       # Skin Tissue Concentration
      cbskin <- abskin/((1-skinvtbc)*vskin)   # SKin Blood Concentration
      askin <- atskin+abskin
      cskin_um <- (atskin+abskin)/vskin
      cskin_mg <- (atskin+abskin)*mw/vskin

      # Muscle
      ctmusc <- atmusc/(muscvtbc*vmusc)       # Muscle Tissue Concentration
      cbmusc <- abmusc/((1-muscvtbc)*vmusc)   # Muscle Blood Concentration
      amusc <- atmusc+abmusc
      cmusc_um <- (atmusc+abmusc)/vmusc
      cmusc_ng <- (atmusc+abmusc)*mw/vmusc

      # Bone
      ctbone <- atbone/(bonevtbc*vbone)       # Bone Tissue Concentration
      cbbone <- abbone/((1-bonevtbc)*vbone)   # Bone Blood Concentration
      abone <- atbone+abbone
      cbone_um <- (atbone+abbone)/vbone
      cbone_ng <- (atbone+abbone)*mw/vbone

      # Brain
      ctbrn <- atbrn/(brnvtbc*vbrn)       # Brain Tissue Concentration
      cbbrn <- abbrn/((1-brnvtbc)*vbrn)   # Brain Blood Concentration
      abrn <- atbrn+abbrn
      cbrn_um <- (atbrn+abbrn)/vbrn
      cbrn_ng <- (atbrn+abbrn)*mw/vbrn

      # Lung
      ctlng <- atlng/(lngvtbc*vlng)       # Lung Tissue Concentration
      cblng <- ablng/((1-lngvtbc)*vlng)   # Lung Blood Concentration
      alng <- atlng+ablng
      clng_um <- (atlng+ablng)/vlng
      clng_ng <- (atlng+ablng)*mw/vlng

      # Heart
      cthrt <- athrt/(hrtvtbc*vhrt)       # Heart Tissue Concentration
      cbhrt <- abhrt/((1-hrtvtbc)*vhrt)   # Heart Blood Concentration
      ahrt <- athrt+abhrt
      chrt_um <- (athrt+abhrt)/vhrt
      chrt_ng <- (athrt+abhrt)*mw/vhrt

      # Kidney
      ctkdn <- atkdn/(kdnvtbc*vkdn)       # Kidney Tissue Concentration
      cbkdn <- abkdn/((1-kdnvtbc)*vkdn)   # Kidney Blood Concentration
      akdn <- atkdn+abkdn
      ckdn_um <- (atkdn+abkdn)/vkdn
      ckdn_ng <- (atkdn+abkdn)*mw/vkdn

      # GI
      ctgi <- atgi/(givtbc*vgi)       # GI Tissue Concentration
      cbgi <- abgi/((1-givtbc)*vgi)   # GI Blood Concentration
      agi <- atgi+abgi
      cgi_um <- (atgi+abgi)/vgi
      cgi_ng <- (atgi+abgi)*mw/vgi

      # Liver
      ctliv <- atliv/(livvtbc*vliv)       # Liver Tissue Concentration
      cbliv <- abliv/((1-livvtbc)*vliv)   # Liver Blood Concentration
      aliv <- atliv+abliv
      cliv_um <- (atliv+abliv)/vliv
      cliv_ng <- (atliv+abliv)*mw/vliv

      # Rapidly Perfused Tissue
      ctrpf <- atrpf/(rpfvtbc*vrpf)       # Rapidly Perfused Tissue Tissue Concentration
      cbrpf <- abrpf/((1-rpfvtbc)*vrpf)   # Rapidly Perfused Tissue Blood Concentration
      arpf <- atrpf+abrpf
      crpf_um <- (atrpf+abrpf)/vrpf
      crpf_ng <- (atrpf+abrpf)*mw/vrpf

      # Slowly Perfused Tissue
      ctspf <- atspf/(spfvtbc*vspf)       # Slowly Perfused Tissue Tissue Concentration
      cbspf <- abspf/((1-spfvtbc)*vspf)   # Slowly Perfused Tissue Blood Concentration
      aspf <- atspf+abspf
      cspf_um <- (atspf+abspf)/vspf
      cspf_ng <- (atspf+abspf)*mw/vspf

      #Blood plasma(venous)
      cv <- (qfat*cbfat + qskin*cbskin + qmusc*cbmusc +
               qbone*cbbone + qbrn*cbbrn + qlng*cblng +
               qhrt*cbhrt + qkdn*cbkdn + qvliv*cbliv +
               qrpf*cbrpf + qspf*cbspf)/qcp

      ###################### EXPOSURE CALCULATIONS ##############################

      # Oral and Drinking Water Exposure


      #rate of oral and drinking water reductions
      rodose <- -ka*odose
      rddose <- -ka*ddose
      # track tot dose as a state variable to it can plotted effectivly at the end
      rtotodose <- 0
      rtotddose <- 0

      # dose available in gut lumen at each time point
      dose_in_gut <- ka*(odose+ddose)

      #change in amount of dose absorbed
      available_dose <- dose_in_gut                       # chemical available for absorbtion after interohepatic circulation
      raabsgut <- fa * available_dose                     # amount absorbed per time step
      rnabsgut <- (1-fa)*available_dose                   # amount not absorbed per time step
      # Inhalation Exposure
      rinhswch <- 0
      rinh <- inhswch*qalv*cinh                           #amount inhaled per time in lung tissue compartment
      rexh <- inhswch*qalv*cpls/pair                      #amount exhaled per time from lung tissue compartment

      # Inravenous Exposure
      rivswch <- 0
      riv <- ivswch * ivdose                              #IV dose per time

      ###################### RATE OF CHANGE CALCULATIONS #########################

      # fat
      rabfat <- qfat*cpls-qfat*cbfat + pafat*ctfat/pfat - pafat*cbfat
      ratfat <- pafat*cbfat - pafat*ctfat/pfat

      # lng
      #rablng <- qlng*cpls -qlng*cblng + palng*ctlng/plng - palng*cblng
      #ratlng <- palng*cblng - palng*ctlng/plng

      #skin
      rabskin <- qskin*cpls- qskin*cbskin + paskin*ctskin/pskin - paskin*cbskin
      ratskin <- paskin*cbskin - paskin*ctskin/pskin

      #muscle
      rabmusc <- qmusc*cpls- qmusc*cbmusc + pamusc*ctmusc/pmusc - pamusc*cbmusc
      ratmusc <- pamusc*cbmusc - pamusc*ctmusc/pmusc

      #bone
      rabbone <- qbone*cpls- qbone*cbbone + pabone*ctbone/pbone - pabone*cbbone
      ratbone <- pabone*cbbone - pabone*ctbone/pbone

      #brain
      rabbrn <- qbrn*cpls- qbrn*cbbrn + pabrn*ctbrn/pbrn - pabrn*cbbrn
      ratbrn <- pabrn*cbbrn - pabrn*ctbrn/pbrn

      #lung
      rablng <- qlng*cpls- qlng*cblng + palng*ctlng/plng - palng*cblng + rinh - rexh
      ratlng <- palng*cblng - palng*ctlng/plng

      #heart
      rabhrt <- qhrt*cpls- qhrt*cbhrt + pahrt*cthrt/phrt - pahrt*cbhrt
      rathrt <- pahrt*cbhrt - pahrt*cthrt/phrt

      #kidney
      rvolurine <- uflw*bw                                              # rate of urine generation
      rauexc <- gfr*wsol*(1-res)*ctkdn/pkdn                          # rate of change for amount of chem urinary excretion
      rabkdn <- qkdn*cpls- qkdn*cbkdn + pakdn*ctkdn/pkdn - pakdn*cbkdn
      ratkdn <- pakdn*cbkdn - pakdn*ctkdn/pkdn - rauexc

      #GI
      rabgi <- qgi*cpls - qgi*cbgi + pagi*ctgi/pgi - pagi*cbgi+ raabsgut
      ratgi <- pagi*cbgi - pagi*ctgi/pgi

      #Liver
      rametliv1 <- vkm1 * (ctliv*fupls/pliv)                                 # amount metabolism in the liver linear
      rametliv2 <- vmaxliv*(ctliv*fupls /pliv)/((ctliv /pliv) + kmliv)      # amount metabolised in the liver saturable
      rabliv <- qaliv*cpls - qvliv*cbliv + qgi*cbgi + paliv*ctliv/pliv - paliv*cbliv
      ratliv <- paliv*cbliv- paliv*ctliv/pliv - rametliv1 - rametliv2

      #Rapidly Perfused Tissue
      rabrpf <- qrpf*cpls- qrpf*cbrpf + parpf*ctrpf/prpf - parpf*cbrpf
      ratrpf <- parpf*cbrpf - parpf*ctrpf/prpf

      #Slowly Perfused Tissue
      rabspf <- qspf*cpls- qspf*cbspf + paspf*ctspf/pspf - paspf*cbspf
      ratspf <- paspf*cbspf - paspf*ctspf/pspf


      #Blood
      raclbld <- kbld*cpls*fupls                                              # generic first order clearence in blood
      rabld <- qcp*(cv-cpls) + riv - raclbld



      ################################ MASS BALANCE ##############################

      totdose <- totodose + totddose + ainh + aiv
      totbody <- abld + afat + askin + amusc + abone + abrn + alng + ahrt + agi + aliv + akdn + arpf + aspf  +odose +ddose
      totclear <- ametliv1+ametliv2 + aclbld + auexc + anabsgut + aexh
      tmass <- totdose - totbody - totclear

      # return all variables
      # the order would be
      # Exposure relate rates
      # compartment Rate
      # Clearance from compartments
      # Concentrations
      # Balance
      list(c(rinhswch,rinh,rexh,rtotodose,rodose,rtotddose,rddose,raabsgut,rivswch,riv,                        #exposure related
             rabld,rabfat,ratfat,rabskin,ratskin,rabmusc,ratmusc,rabbone,ratbone,rabbrn,ratbrn,rablng,ratlng, # Compartments
             rabhrt,rathrt,rabgi,ratgi,rabliv,ratliv,rabkdn,ratkdn,rabrpf,ratrpf,rabspf,ratspf,               # Compartments
             rametliv1,rametliv2,raclbld,rauexc,rnabsgut),                                                              # Clearance
           "afat"=afat,"askin"=askin,"amusc"=amusc,
           "abone"=abone,"abrn"=abrn,"alng"=alng,
           "ahrt"=ahrt,"akdn"=akdn,"agi"=agi,
           "aliv"=aliv,"arpf"=arpf,"aspf"=aspf,
           "cpls" = cpls, "cv"=cv,
           "cfat_um"=cfat_um,"ctfat"=ctfat, "cbfat"=cbfat,
           "cskin_um"=cskin_um,"ctskin"=ctskin, "cbskin"=cbskin,
           "cmusc_um"=cmusc_um,"ctmusc"=ctmusc, "cbmusc"=cbmusc,
           "cbone_um"=cbone_um,"ctbone"=ctbone, "cbbone"=cbbone,
           "cbrn_um"=cbrn_um,"ctbrn"=ctbrn, "cbbrn"=cbbrn,
           "clng_um"=clng_um,"ctlng"=ctlng, "cblng"=cblng,
           "chrt_um"=chrt_um,"cthrt"=cthrt, "cbhrt"=cbhrt,
           "ckdn_um"=ckdn_um,"ctkdn"=ctkdn, "cbkdn"=cbkdn,
           "cgi_um"=cgi_um,"ctgi"=ctgi, "cbgi"=cbgi,
           "cliv_um"=cliv_um,"ctliv"=ctliv, "cbliv"=cbliv,
           "crpf_um"=crpf_um,"ctrpf"=ctrpf, "cbrpf"=cbrpf,
           "cspf_um"=cspf_um,"ctspf"=ctspf, "cbspf"=cbspf,
           "InstInhDose"=rinh,"mbal"=tmass
           )

    }
  )
}
