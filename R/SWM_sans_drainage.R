SWM_sans_drainage <- function(rainfall){

  # Check if rainfall is given as an input
  if(missing(rainfall))
    data(stoc)
  rainfall = stoc

  # Load Fabien's big RData that has basically all the needed stuff to compute REW.
  data(m3)

  # Load the .C executable library compiled back in 2013
  dyn.load(system.file("lib/SWMoutput2013.dll", package="SWMviaOriginalCode"))

  # Take the MCMC output from calibration with TDR that are actually in the big .RData
  coefTr_out=res[[6]]
  ThetaPWP_out=matrix(res[[3]],nrow=LTheta,ncol=niter/thining)
  ThetaFC_out=matrix(res[[4]],nrow=LTheta,ncol=niter/thining)
  ThExp_out=matrix(res[[5]],nrow=N_soil,ncol=niter/thining)

  # Summarise parameters to their median as described in the article.
  ThTr=median(coefTr_out[201:1201])
  ThetaPWPAll=apply(ThetaPWP_out[,201:1201],1,median)
  ThetaFCAll=apply(ThetaFC_out[,201:1201],1,median)
  ThExpAll= median(ThExp_out[,201:1201])#apply(ThExp_out[,201:1201],1,median)

  # Set the number of layers as in Fabien's code
  N_layer=270

  rm(list=c("acceptcoefTrAll", "acceptThExpAll", "acceptThFCAll", "acceptThPWPAll",
            "coefTr_out", "control", "dataObsTube_1", "dataObsTube_10",
            "dataObsTube_12", "dataObsTube_13", "dataObsTube_16", "dataObsTube_18",
            "dataObsTube_19", "dataObsTube_2", "dataObsTube_3", "dataObsTube_4",
            "dataObsTube_5", "dataObsTube_6", "dataObsTube_7", "dataObsTube_8",
            "dataObsTube_9", "Em", "ETP", "EW_calc", "EW_calcAll", "EW_obs",
            "EW_obsAll", "graphTheta", "graphThExp", "i", "I0", "In", "InAll", "j", "jour", "Jour1", "jourAll", "jourMcmc",
            "jourMcmcAll", "jType", "k", "LAI", "LAll", "Lobs", "LObsAll", "LTheta", "lvrais", "m", "McmcM1output", "mod", "N_day",
            "N_dayAll", "N_layerAll", "N_obs", "N_obsAll", "N_probe",
            "N_soil", "niter", "Ntubesoil", "ObsDay", "ObsDayAll", "percREW",
            "percREWAll", "Pluie", "PluieAll", "Prof", "ProfAll",
            "ProfMaxAll", "ProfObs", "R2", "R2All", "res",
            "REW", "REWAll", "Rm", "RMSE", "RMSEAll", "Rn_Js",
            "Rn_MJ", "roots_coucheAll", "T0", "test", "ThetaFC", "ThetaFC_out",
            "ThetaFCAllSoil", "ThetaPWP", "ThetaPWP_out",
            "ThetaPWPAllSoil", "ThExp_out", "ThExpType", "ThFC_out",
            "thining", "ThPWP_out", "Throots_out", "ThTrAll", "Tps",
            "Wmaxtmp", "Wmintmp"))

  data=stoc

  pluie=data$pluie
  ThetaFC=ThetaFCAll[1:13]
  ThetaPWP=ThetaPWPAll[1:13]
  ThExp=ThExpAll

  ProfObs=ProfObsAll[1:13]
  EW_obs=c(rep(0.2,13))
  jour=2
  N_day=length(pluie)
  ObsDay=rep(0,length(pluie));
  ObsDay[2800]=1
  Lobs=13
  Em<-0.64
  Rm<-8.98
  In<-interception( pluie, Em, Rm, p[9], p[10], p[11] )


  I0= 592.1 # valeur moyenne de gyaflux en MJ . m-2 .d-1
  LAI=7.000
  ETP= rep(3.970279, length(data$jour))
  REW=rep(0,N_day)
  Dr=rep(0,N_day)

  REW[1]=1
  R2=0
  RMSE=0
  EW_calc=rep(0,length(as.double(EW_obs)))
  percREW=0




  res1=SWMoutput(ThExp,ThetaPWP,ThetaFC, ThTr, N_layer, pluie, EW_obs, jour, ProfMax, In, roots_couche, p, I0,LAI, ETP,LogL, N_day, ObsDay, L,ProfObs,Lobs, REW,percREW, EW_calc)

  plot(res1[[22]],type="l")



  data$REW=res1[[22]]
  data$SWD=data$REW
  data$SWD[data$SWD>0.3]=0
  data$SWD[data$SWD!=0]=0.3-data$SWD[data$SWD!=0]
  plot(data$SWD)
  str(data )

  index=data
  REWdata=index[,c(1:5,10,11)]
  save(REWdata,file="REW.1978.2013.Rdata")
  write.csv2(REWdata,file="REW.1978.2013.csv")

  return()
}

