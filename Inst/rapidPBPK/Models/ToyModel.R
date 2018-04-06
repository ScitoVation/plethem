library(deSolve)

toyModel <- function(t,state,parameters){
  with(
    as.list(c(state,parameters)), {
    # params_calcs
    vl <- VLC*BW                    			# Liver volume -L
    qc <- QCC*BW^0.74                 		# Cardiac Output - L/hr
    ql <- QLC*qc                        	# Liver blood flow - L/hr
    qt <- qc-ql                      			# Tissue blood flow L/hr
    vb <- VBC*BW                     			# Blood volume -L 
    vt <- BW-vl-vb                 				# Tissue volume - L
    # conc Calcs
    cb = AB/vb
    ct = AT/vt
    cl = AL/vl
    # equations
    dTotDose <- 0
    rdose<- DecrBolusRt * DOSE    
    dDOSE <- -rdose
    dAT <- qt*cb - qt*ct/pt 
    dAUCT<- AT
    dAL <- ql*cb - ql*cl/pl+rdose
    dAUCL <- AL
    dAB <- qt*ct/pt+ql*cl/pl - qc*AB/vb
    dAUCB<- AB
    # MAss BAL 
    bal <- TotDose - (DOSE+AB+AL+AT)
    
    
    list(c(dTotDose,dDOSE,dAT,dAL,dAB,dAUCT,dAUCL,dAUCB),"CB"=cb,"CL"=cl,"CT"=ct,"BAL"=bal)
  })
}