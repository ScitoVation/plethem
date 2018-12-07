source('LifecoursePhysiology.R')

ages = seq(0, 60, 0.5)


for(gender in c(PLETHEM_GENDER_MALE, PLETHEM_GENDER_FEMALE))
{
  bw = {}
  bh = {}
  bsa = {}
  bmi = {}
  qcc = {}
  vbl = {}
  hct = {}
  vadi = {}
  vbone = {}
  vbrn = {}
  vgon = {}
  vhrt = {}
  vint = {}
  vkid = {}
  vliv = {}
  vlng = {}
  vpan = {}
  vskn = {}
  vspl = {}
  vsto = {}
  vthy = {}
  vrem = {}
  vmus = {}
  vall = {}
  
  qadi = {}
  qbone = {}
  qbrn = {}
  qgon = {}
  qhrt = {}
  qint = {}
  qkid = {}
  qliv = {}
  qliva = {}
  qlivs = {}
  qmus = {}
  qpan = {}
  qskn = {}
  qspl = {}
  qsto = {}
  qthy = {}
  qrem = {}
  qco  = {}
  vr   = {}
  ur   = {}
  gfr  = {}
  
  for(year in ages)
  {
    bw    = c(bw, getLifecourseBodyWeight(year, gender))
    bh    = c(bh, getLifecourseBodyHeight(year, gender))
    bsa   = c(bsa, getLifecourseBodySurfaceArea(year, gender))
    bmi   = c(bmi, getLifecourseBodyMassIndex(year, gender))
    hct   = c(hct, getLifecourseHematocrit(year, gender))
    vbl   = c(vbl, getLifecourseBloodVolume(year, gender))
    vadi  = c(vadi, getLifecourseAdiposeVolume(year, gender))
    vbone = c(vbone, getLifecourseBoneVolume(year, gender))
    vbrn  = c(vbrn, getLifecourseBrainVolume(year, gender))
    vgon  = c(vgon, getLifecourseGonadVolume(year, gender))
    vhrt  = c(vhrt, getLifecourseHeartVolume(year, gender))
    vint  = c(vint, getLifecourseIntestineVolume(year, gender))
    vkid  = c(vkid, getLifecourseKidneyVolume(year, gender))
    vliv  = c(vliv, getLifecourseLiverVolume(year, gender))
    vlng  = c(vlng, getLifecourseLungVolume(year, gender))
    vpan  = c(vpan, getLifecoursePancreasVolume(year, gender))
    vskn  = c(vskn, getLifecourseSkinVolume(year, gender))
    vspl  = c(vspl, getLifecourseSpleenVolume(year, gender))
    vsto  = c(vsto, getLifecourseStomachVolume(year, gender))
    vthy  = c(vthy, getLifecourseThymusVolume(year, gender))
    vrem  = c(vrem, getLifecourseRemainingVolume(year, gender))
    vmus  = c(vmus, getLifecourseMuscleVolume(year, gender))
    vall  = c(vall, getLifecourseTissueVolumeSum(year, gender))
    
    qadi  = c(qadi, getLifecourseAdiposePerfusion(year, gender))
    qbone = c(qbone, getLifecourseBonePerfusion(year, gender))
    qbrn  = c(qbrn, getLifecourseBrainPerfusion(year, gender))
    qgon  = c(qgon, getLifecourseGonadPerfusion(year, gender))
    qhrt  = c(qhrt, getLifecourseHeartPerfusion(year, gender))
    qint  = c(qint, getLifecourseIntestinePerfusion(year, gender))
    qkid  = c(qkid, getLifecourseKidneyPerfusion(year, gender))
  
    qliv  = c(qliv, getLifecourseLiverTotalPerfusion(year, gender))
    qliva  = c(qliva, getLifecourseLiverArterialPerfusion(year, gender))
    qlivs  = c(qlivs, getLifecourseLiverSplancnicPerfusion(year, gender))
    
    qmus  = c(qmus, getLifecourseMusclePerfusion(year, gender))
    qpan  = c(qpan, getLifecoursePancreasPerfusion(year, gender))
    qskn  = c(qskn, getLifecourseSkinPerfusion(year, gender))
    qspl  = c(qspl, getLifecourseSpleenPerfusion(year, gender))
    qsto  = c(qsto, getLifecourseStomachPerfusion(year, gender))
    qthy  = c(qthy, getLifecourseThymusPerfusion(year, gender))
    qrem  = c(qrem, getLifecourseRemainingPerfusion(year, gender))
    qco   = c(qco, getLifecourseCardiacOutput(year, gender))
    
    vr    = c(vr, getLifecourseVentilationRate(year, gender))
    ur    = c(ur, getLifecourseUrineProductionRate(year, gender))
    gfr   = c(gfr, getLifecourseGlomerularFiltrationRate(year, gender))
  }
  
  if(gender == 1) genname = "Male" else genname = "Female"
  
  par(mfrow=c(3, 2))
  
  plot(ages, bw, type="l", main="Body Weight (kg)")
  plot(ages, bh, type="l", main="Body Height (cm)")
  plot(ages, bsa, type="l", main="Body Surface Area (m^2)")
  plot(ages, bmi, type="l", main="Body Mass Index (kg/m^2)")
  plot(ages, hct, type="l", main="Hematocrit")

  par(mfrow=c(3, 2))
  
  plot(ages, vbl, type="l", main="Blood Volume (L)")
  plot(ages, vadi, type="l", main="Adipose Volume (L)")
  plot(ages, vbone, type="l", main="Bone Volume (L)")
  plot(ages, vbrn, type="l", main="Brain Volume (L)")
  plot(ages, vgon, type="l", main="Gonad Volume (L)")
  plot(ages, vhrt, type="l", main="Heart Volume (L)")

  par(mfrow=c(3, 2))
  
  plot(ages, vint, type="l", main="Intestine Volume (L)")
  plot(ages, vkid, type="l", main="Kidney Volume (L)")
  plot(ages, vliv, type="l", main="Liver Volume (L)")
  plot(ages, vlng, type="l", main="Lung Volume (L)")
  plot(ages, vpan, type="l", main="Pancreas Volume (L)")
  plot(ages, vskn, type="l", main="Skin Volume (L)")

  par(mfrow=c(3, 2))

  plot(ages, vspl, type="l", main="Spleen Volume (L)")
  plot(ages, vsto, type="l", main="Stomach Volume (L)")
  plot(ages, vthy, type="l", main="Thymus Volume (L)")
  plot(ages, vrem, type="l", main="Remaining Tissue Volume (L)")
  plot(ages, vmus, type="l", main="Muscle Volume (L)")
  plot(ages, vall, type="l", main="Sum of All Volumes (L)")

  par(mfrow=c(3, 2))
  
  plot(ages, qadi, type="l", main="Adipose Perfusion (L/h)")
  plot(ages, qbone, type="l", main="Bone Perfusion (L/h)")
  plot(ages, qbrn, type="l", main="Brain Perfusion (L/h)")
  plot(ages, qgon, type="l", main="Gonad Perfusion (L/h)")
  plot(ages, qhrt, type="l", main="Heart Perfusion (L/h)")
  plot(ages, qint, type="l", main="Intestine Perfusion (L/h)")

  par(mfrow=c(3, 2))

  plot(ages, qkid, type="l", main="Kidney Perfusion (L/h)")
  plot(ages, qmus, type="l", main="Muscle Perfusion (L/h)")
  plot(ages, qpan, type="l", main="Pancreas Perfusion (L/h)")
  plot(ages, qskn, type="l", main="Skin Perfusion (L/h)")
  plot(ages, qspl, type="l", main="Spleen Perfusion (L/h)")

  par(mfrow=c(3, 2))
  
  plot(ages, qsto, type="l", main="Stomach Perfusion (L/h)")
  plot(ages, qthy, type="l", main="Thymus Perfusion (L/h)")
  plot(ages, qrem, type="l", main="Remaining Perfusion (L/h)")
  plot(ages, qco, type="l", main="CardiacOutput (L/h)")
  plot(ages, vr, type="l", main="VentilationRate (L/h)")

  par(mfrow=c(3, 2))
  plot(ages, qliv, type="l", main="Liver Total Perfusion (L/h)")
  plot(ages, qliva, type="l", main="Liver Arterial Perfusion (L/h)")
  plot(ages, qlivs, type="l", main="Liver Splancnic Perfusion (L/h)")
  plot(ages, ur, type="l", main="Urine Production (ml/h)")
  plot(ages, gfr, type="l", main="GFR (L/h)")
  
}

