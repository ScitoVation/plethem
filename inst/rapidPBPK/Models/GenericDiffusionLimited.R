# Modified version of the Cefic Indus chem fate model
# Modified to include rest of body compartment
# Modified to have all compartments as diffusion limited


library(deSolve)

genericCarbarylModel <- function(t,state,parameters){
  with(
    as.list(c(state,parameters)),{
      # PARAMETER SCALING
      #TISSUE VOLUME CHECK
      VOLTOTALC <- VOLBLOODC + VOLADIPC+ VOLBRAINC + VOLLIVERC +VOLRESTC
      
      
      
      
      # SCALED TISSUE VOLUMES (L)
      VOLBLOOD <- VOLBLOODC *(0.85/VOLTOTALC)* BODYWT    # L; BLOOD
      VOLADIP <- VOLADIPC *(0.85/VOLTOTALC)* BODYWT     # L; ADIPOSE TISSUE
      VOLBRAIN <- VOLBRAINC *(0.85/VOLTOTALC)* BODYWT    # L; BRAIN TISSUE
      VOLLIVER <- VOLLIVERC *(0.85/VOLTOTALC)* BODYWT    # L; LIVER TISSUE
      VOLREST <-  VOLRESTC*(0.85/VOLTOTALC)*BODYWT
      VOLPLS <- VOLBLOOD*(1-HCT)
      VOLRBC <- VOLBLOOD*HCT
      
      #BLOOD FLOW CHECK
      FRTOTALC <- FRADIPC + FRBRAINC  + FRLIVC + FRRESTC
      
      # SCALED BLOOD FLOWS (L / HR)
      CARDOUTP <- CARDOUTPC * (1-HCT)      # L/HR; CARDIAC OUTPUT # use different type of QC
      QADIP <- FRADIPC *(1/FRTOTALC)* CARDOUTP        # L/HR; ADIPOSE BLOOD FLOW
      QBRAIN <- FRBRAINC *(1/FRTOTALC)* CARDOUTP       # L/HR; BRAIN BLOOD FLOW
      QLIV <- FRLIVC * (1/FRTOTALC)*CARDOUTP      # L/HR; LIVER ARTERIAL BLOOD FLOW
      QREST <- FRRESTC* (1/FRTOTALC)*CARDOUTP
      
      #permiability constants
      
      PAF = PAFC* (VOLADIP ^0.75)	                # fat
      PAR = PARC* (VOLREST ^0.75)	                # remaining
      PAL = PALC* (VOLLIVER ^0.75)                	# liver
      PAB = PABC* (VOLBRAIN ^0.75)                	# brain
      
      #carbaryl metabolism
      
      VKM1  = VKM1C  * VOLLIVER					          	# carbaryl CLint in liver
      VKM2  = VKM2C  * (VOLBLOOD*(1-HCT))           # carbaryl CLint in plasma, VPLS= (VBLD*(1-HCT))
      KenB  = KenBC  * BODYWT^(-0.25)	          	# carbaryl rate of excretion from plasma to urine
      
      # TIME CONVERSIONS
      HOURS<-T
      DAYS<- 0.0416666666666667*T
      MINUTES<- 60.0*T
      WEEKS<-0.142857142857143*DAYS
      YEARS<-0.000114077116130504*HOURS
      MONTHS<-0.0328542094455852*DAYS
      
      #concentrations
      
      #LIVER
      CVL = AVL/(VTBC*VOLLIVER)
      CL = AL /((1-VTBC)*VOLLIVER)
      CLIV_uM=(AVL+AL)/VOLLIVER																	
      CLIV=(AVL+AL)*MW*0.001/VOLLIVER
      #FAT
      CVF = AVF/(VTBC*VOLADIP)
      CF= AF /((1-VTBC)*VOLADIP)
      CFAT_uM=(AVF+AF)/VOLADIP																	
      CFAT=(AVF+AF)*MW*0.001/VOLADIP
      #REST OF THE BODY
      CVR = AVR/(VTBC*VOLREST)
      CR= AR /((1-VTBC)*VOLREST)
      CREST_uM=(AVR+AR)/VOLREST																	
      CREST=(AVR+AR)*MW*0.001/VOLREST
      #BRAIN
      CVB = AVB/(VTBC*VOLBRAIN)
      CB= AB /((1-VTBC)*VOLBRAIN)
      CBRN_uM=(AVB+AB)/VOLBRAIN																	
      CBRN=(AVB+AB)*MW*0.001/VOLBRAIN
      #BLOOD
      CPLS = APLS/VOLPLS
      CV = (QLIV*CVL+QADIP*CVF+QREST*CVR+QBRAIN*CVB)/CARDOUTP
      CPL_um = CPLS
      CPL = CPLS*MW*0.001
      
      #Oral Exposure
      RTOTDOSE <- 0
      avlbl_dose<- DecrBolusRt * DOSE
      RODOSE <- -avlbl_dose
      
      #Equations
      RAVL = QLIV*(CPLS - CVL) + avlbl_dose +PAL*(CL/PLIV - CVL) # liver blood
      RAL =  PAL*(CVL-CL/PLIVER)-LRAM                            #liver tissue
      LRAM =  VKM1*(CL*FuLIV)                                    #liver metabolism
      RAVF = QADIP*(CPLS - CVF) + PAF*(CF/PADIP - CVF)	         #fat blood
      RAF = PAF*(CVF-CF/PADIP)                                   #fat tissue
      RAVR = QREST*(CPLS - CVR) + PAR*(CR/PREST - CVR)	         #Rest of the body tissue blood
      RAR = PAR*(CVR-CR/PREST)                                   #REst of the body tissue
      RAVB = QBRAIN*(CPLS - CVB) + PAB*(CB/PBRAIN - CVB)	       #brain blood
      RAB = PAB*(CVB-CB/PBRAIN)                                  #brain tissue
      RAPLS = CARDOUTP *(CV-CPLS)-BRAM                           #blood plasma
      BRAM = VKM2*(CPLS*FuPLS)                                   #blood metabolism
      
      # Area Under Curves
      RAUCBRAIN <- CBRN
      RAUCLIVER <- CLIV
      RAUCADIPOSE <- CFAT
      RAUCBLOOD <- CPL
      
      # MAss Balance
      TOTBOD<-AVL+AL+AVF+AF+AVR+AR+AVB+AB+APLS
      TOTCLEAR<- ALRAM+ABRAM 
      TMASS<- TOTDOSE-TOTBOD - TOTCLEAR
      
      list(c(RAVL, RAL,LRAM, RAVF, RAF, RAVR, RAR, RAVB, RAB, RAPLS, BRAM,RAUCBRAIN, RAUCLIVER, RAUCADIPOSE, RAUCBLOOD),
           "cvl"= CVL,"cl"=CL,"cliv_um"=CLIV_uM,"cliv"=CLIV,"cvf"= CVF,"cf"=CF,"cfat_um"=CFAT_uM,"cfat"=CFAT,"cvr"=CVR,"cr"=CR,
           "crest_um"=CREST_uM,"crest"=CREST,"cvb"=CVB,"cb"=CB,"cbrn_um"=CBRN_uM,"cbrn"=CBRN,"cpls"=CPLS,"cv"=CV,"cpl_um"=CPL_um,
           "cpl"=CPL,"bal"=TMASS,"dose"=TOTDOSE) 
    }
  )
}

genericModel <- function(t,state,parameters){
  with(
    as.list(c(state,parameters)), {
      # PARAMETER SCALING
      #TISSUE VOLUME CHECK
      VOLTOTALC <- VOLBLOODC + VOLADIPC + VOLBONEC + VOLBRAINC + VOLHEARTC + VOLKIDNEYC + VOLINTESTC + VOLLIVERC + VOLLUNGSC + VOLMUSCLEC + VOLSKINC + VOLMARROWC
      
      #BLOOD FLOW CHECK
      FRTOTALC <- FRADIPC + FRBONEC + FRBRAINC + FRHEARTC + FRKIDNEYC + FRLIVVENC +FRLIVARTC + FRLUNGC + FRMUSCLEC + FRSKINC + FRMARROWC
      
      # SCALED PULMONARY VENTILATION RATE
      ALVVENT <- ALVVENTC * BODYWT   # L/HR; SCALED PULMONARY VENTILATION RATE
      
      # SCALED TISSUE VOLUMES (L)
      VOLBLOOD <- VOLBLOODC * BODYWT    # L; BLOOD
      VOLBLUNGART <- FRART * VOLBLOOD      # L; ARTERIAL BLOOD
      VOLBLUNGVEN <- FRVEN * VOLBLOOD      # L; VENOUS BLOOD
      VOLADIP <- VOLADIPC * BODYWT     # L; ADIPOSE TISSUE
      VOLBONE <- VOLBONEC * BODYWT     # L; BONE TISSUE
      VOLBRAIN <- VOLBRAINC * BODYWT    # L; BRAIN TISSUE
      VOLHEART <- VOLHEARTC * BODYWT    # L; HEART TISSUE
      VOLKIDNEY <- VOLKIDNEYC * BODYWT   # L; KIDNEY TISSUE
      VOLINTEST <- VOLINTESTC * BODYWT   # L; INTESTINE TISSUE
      VOLLIVER <- VOLLIVERC * BODYWT    # L; LIVER TISSUE
      VOLLUNGS <- VOLLUNGSC * BODYWT    # L; LUNG TISSUE
      VOLMUSCLE <- VOLMUSCLEC * BODYWT   # L; MUSCLE TISSUE
      VOLSKIN <- VOLSKINC * BODYWT     # L; SKIN TISSUE
      VOLMARROW <- VOLMARROWC * BODYWT   # L; MARROW TISSUE
      
      # SCALED BLOOD FLOWS (L / HR)
      CARDOUTP <- CARDOUTPC * BODYWT        # L/HR; CARDIAC OUTPUT
      QADIP <- FRADIPC * CARDOUTP        # L/HR; ADIPOSE BLOOD FLOW
      QBONE <- FRBONEC * CARDOUTP        # L/HR; BONE BLOOD FLOW
      QBRAIN <- FRBRAINC * CARDOUTP       # L/HR; BRAIN BLOOD FLOW
      QHEART <- FRHEARTC * CARDOUTP       # L/HR; HEART BLOOD FLOW
      QKIDNEY <- FRKIDNEYC * CARDOUTP      # L/HR; KIDNEY BLOOD FLOW
      QLIVVEN <- FRLIVVENC * CARDOUTP      # L/HR; LIVER VENOUS BLOOD FLOW
      QLIVART <- FRLIVARTC * CARDOUTP      # L/HR; LIVER ARTERIAL BLOOD FLOW
      QLUNG <- FRLUNGC * CARDOUTP        # L/HR; LUNG BLOOD FLOW
      QMUSCLE <- FRMUSCLEC * CARDOUTP      # L/HR; MUSCLE BLOOD FLOW
      QSKIN <- FRSKINC * CARDOUTP        # L/HR; SKIN BLOOD FLOW
      QMARROW <- FRMARROWC * CARDOUTP      # L/HR; MARROW BLOOD FLOW
      
      # TIME CONVERSIONS
      HOURS<-T
      DAYS<- 0.0416666666666667*T
      MINUTES<- 60.0*T
      WEEKS<-0.142857142857143*DAYS
      YEARS<-0.000114077116130504*HOURS
      MONTHS<-0.0328542094455852*DAYS
      
      # Concentrations
      CMUSCLE<-AMUSCLE/VOLMUSCLE
      CVMUSCLE<-CMUSCLE/PMUSCLE
      CSKIN<-ASKIN/VOLSKIN
      CVSKIN <- CSKIN / PSKIN 
      CBONE<-ABONE/VOLBONE
      CVBONE<-CBONE/PBONE
      CMARROW<-AMARROW/VOLMARROW
      CVMARROW<-CMARROW/PMARROW
      CADIPOSE<-AADIPOSE/VOLADIP
      CVADIPOSE <-  CADIPOSE / PADIP   
      CSTOM<-ASTOM/VOLINTEST
      CVSTOM<-CSTOM/PSTOM
      CLIVER<-ALIVER/VOLLIVER
      CVLIVER <-  CLIVER / PLIVER 
      CMET<-AMET/VOLLIVER
      CHEART<-AHEART/VOLHEART
      CVHEART <- CHEART / PHEART 
      CLUNG<-ALUNG/VOLLUNGS
      CVLUNG <- CLUNG / PLUNG  
      CINH<-0#*CEXP/(MW*RESPPROT)
      
      CKIDNEY<-AKIDNEY/VOLKIDNEY
      CVKIDNEY<-CKIDNEY/PKIDNEY
      CEXC<-AEXC/URINVOL
      CBRAIN<-ABRAIN/VOLBRAIN
      CVBRAIN<-CBRAIN/PBRAIN
      CVEN<-AVEN/VOLBLUNGVEN
      CART<- AART/VOLBLUNGART
      CALV<-CART/RCBA
      #MASSINITABS<-2*DENS*KPW0/FPART - 2*DENS*KPW0*STRATCORN/(FPART*MASSLOAD)
      #RTSCMC<-2*STRATCORN/MAXCAP
      #SKINFLUX<-1000.0*RAMTABS*SKINAREA/MW
      
      #Oral Exposure
      RTOTDOSE <- 0
      avlbl_dose<- DecrBolusRt * DOSE
      RODOSE <- -avlbl_dose
      # Amounts
      # muscle
      RAMUSCLE<-CART*QMUSCLE - CVMUSCLE*QMUSCLE
      #skin
      RASKIN<-CART*QSKIN - CVSKIN*QSKIN
      # bone
      RABONE<-CART*QBONE - CVBONE*QBONE
      #Marrow
      RAMARROW<-CART*QMARROW - CVMARROW*QMARROW
      # fat
      RAADIPOSE<-CART*QADIP - CVADIPOSE*QADIP
      # Gastric
      RASTOM<-avlbl_dose + CART*QLIVVEN - CVSTOM*QLIVVEN
      # Liver Metab
      RAMET<-CVLIVER*VMAX/(CVLIVER + KM)
      #LIVER
      RALIVER<-CART*QLIVART - CVLIVER*QLIVART - CVLIVER*QLIVVEN + CVSTOM*QLIVVEN - RAMET
      # Heart
      RAHEART<-CART*QHEART - CVHEART*QHEART
      # Lung
      RALUNG<-CART*QLUNG - CVLUNG*QLUNG
      # Exhaled from Lung
      RAEXH<-ALVVENT*CALV
      # Inhaled into Lung
      RAINH<-ALVVENT*CINH
      # Kidney Exceretion
      RAEXC<-CVKIDNEY*FRWSOL*GLOMFILTR*QKIDNEY*REMOVKDN
      # Kidney
      RAKIDNEY<-CART*QKIDNEY - CVKIDNEY*QKIDNEY - RAEXC
      
      # Urine Generation
      RURINVOL<-0.0416666666666667*BODYWT*URINFLOW
      #Brain
      RABRAIN<-CART*QBRAIN - CVBRAIN*QBRAIN
      # Arterial Blood
      RAART<--ALVVENT*CALV + ALVVENT*CINH - CART*CARDOUTP + CVEN*CARDOUTP
      # venous Blood
      RAVEN<-CVADIPOSE*QADIP + CVBONE*QBONE + CVBRAIN*QBRAIN + CVHEART*QHEART + CVKIDNEY*QKIDNEY + CVLIVER*QLIVART + CVLIVER*QLIVVEN + CVLUNG*QLUNG + CVMARROW*QMARROW + CVMUSCLE*QMUSCLE + CVSKIN*QSKIN - CVEN*CARDOUTP
      
      # Area Under Curves
      RAUCMUSCLE <- CMUSCLE
      RAUCBRAIN <- CBRAIN
      RAUCLIVER <- CLIVER
      RAUCBONE <- CBONE
      RAUCLUNG <- CLUNG
      RAUCSKIN <- CSKIN
      RAUCMARROW <- CMARROW
      RAUCVEN  <- CVEN
      RAUCADIPOSE <- CADIPOSE
      RAUCKIDNEY <- CKIDNEY
      RAUCSTOM <- CSTOM
      RAUCART <- CART
      RAUCHEART <- CHEART
      
      # MAss Balance
      TOTBOD<-AADIPOSE + AART + ABONE + ABRAIN + AHEART + AKIDNEY + ALIVER + ALUNG + AMARROW + AMUSCLE + DOSE + ASKIN + ASTOM + AVEN
      TOTCLEAR<-AEXC + AEXH + AMET
      TMASS<- TOTDOSE-TOTBOD - TOTCLEAR
      
      list(c(RAMUSCLE,RAEXH,RABRAIN,RAEXC,RALIVER,RABONE,RALUNG,RASKIN,RAMARROW,RAINH,RAVEN,RURINVOL,RAADIPOSE,RAKIDNEY,RASTOM,RAMET,RTOTDOSE,RODOSE,RAART,RAHEART,
             RAUCMUSCLE,RAUCBRAIN,RAUCLIVER,RAUCBONE,RAUCLUNG,RAUCSKIN,RAUCMARROW,RAUCVEN,RAUCADIPOSE,RAUCKIDNEY,RAUCSTOM,RAUCART,RAUCHEART),
           "cmusc"=CMUSCLE,"cbrn"=CBRAIN,"cliv"=CLIVER,"cbne"=CBONE,"clng"=CLUNG,"cskn"=CSKIN,"cmarr"=CMARROW,"cven"=CVEN,"cadip"=CADIPOSE,"ckdn"=CKIDNEY,
           "cstm"=CSTOM,"cart"=CART,"chrt"=CHEART,"bal"=TMASS
           )
    })
}