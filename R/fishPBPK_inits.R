#'@export
fishPBPK_initParms <- function(newParms = NULL) {
  parms <- c(
    bw = 0,
    qc = 0,
    qg = 0,
    vfatc = 0,
    qfatc = 0,
    pfat = 0,
    vlivc = 0,
    qlivc = 0,
    pliv = 0,
    vkdnc = 0,
    qkdnc = 0,
    pkdn = 0,
    vrpfc = 0,
    qrpfc = 0,
    prpf = 0,
    vspfc = 0,
    qspfc = 0,
    pspf = 0,
    frspfkdn = 0.4,
    vfat = 0,
    vkdn = 0,
    vliv = 0,
    vrpf = 0,
    vspf = 0,
    qfat = 0,
    qkdn = 0,
    qliv = 0,
    qrpf = 0,
    qspf = 0,
    vmax = 0,
    km = 1e-10,
    cins = 0,
    pbldw = 1e10,
    gul = 1
  )

  if (!is.null(newParms)) {
    if (!all(names(newParms) %in% c(names(parms)))) {
      stop("illegal parameter name")
    }
    parms[names(newParms)] <- newParms
  }

  parmsfishPBPK <- within(as.list(parms), {
  })
  out <- .C("getParmsfishPBPK",  as.double(parms),
            out=double(length(parms)),
            as.integer(length(parms)))$out
  names(out) <- names(parms)
  out
}

fishPBPK_Outputs <- c(
    "cv",
    "ca",
    "mbal"
)
#'@export
fishPBPK_initStates <- function(parms, newStates = NULL) {
  Y <- c(
    cfat = 0.0,
    cliv = 0.0,
    cspf = 0.0,
    crpf = 0.0,
    ckdn = 0.0,
    cmet = 0.0,
    ains = 0.0,
    insswch = 0.0
  )

  if (!is.null(newStates)) {
    if (!all(names(newStates) %in% c(names(Y)))) {
      stop("illegal state variable name in newStates")
    }
    Y[names(newStates)] <- newStates
  }

.C("initStatefishPBPK", as.double(Y));
Y
}
