#' Run the Original Code of SWM
#'
#' This function allow to run exactly the same code as Fabien does to output REW
#' values. Note that the C code called to run the simulations, and the
#' parameters of default simulations (used so far) are not exactly what is
#' decribed in the corresponding article. A document explaining the differences
#' between article and code will be edited, and available upon demand to
#' page.nino@@gmail.com or geraldine.derroire@@ecofog.gf
#'
#' @param rainfall_data data.frame, but defaults to NA. Must contain daily
#'   rainfall (if you have monthly rates, see convert_rainfall function), along
#'   with a column containing dates (which format is specified with argument
#'   date_format) or three column named "year","month" and "day" containing the
#'   appropriate infos. If let to NA, a default dataset is automatically loaded
#'   and the model is run.
#' @param rainfall_col character, but defaults to NA. If the column containing
#'   rainfall in your dataset is named "pluie" as in Fabien's data, no need to
#'   specify it.
#' @param date_col character, but defaults to NA. If this information is
#'   contained in three columns names "year", "month", and "day" as in Fabien's
#'   dataset, no need to specify it.
#' @param date_format single character, but defaults to NA. To be let to default
#'   if date_col is also let to default. If your dataset contains a date column,
#'   please 1- make sure the date format is strictly homogenous accross your
#'   dataset ; and 2- specify it with three letters (lubridate's formalism): 'y'
#'   for year, 'm' for month and 'd' for day, e.g.: 'ymd'.
#' @param drainage logical, defaults to FALSE. If TRUE, the SWM version "with
#'   drainage" is run. I think the only difference is that deep drainage is also
#'   outputed. To confirm with Fabien.
#' @param ...
#'
#' @return A data.frame made from the input (or default) rainfall data,
#'   containing the following columns: * date:The original or generated date
#'   (according to how this information is included in the input dataset, see
#'   above), parsed with lubridate according to its format (either specified
#'   with date_format if you input complete dates as characters, or defaultly
#'   working if your input dates are split into three columns, see above) *
#'   year, month and day:The original or generated year, month and day for each
#'   simulated date (according to how this information is included in the input
#'   dataset, see above) * julian_day:The julian date corresponding to each
#'   date, with origin set to 10-10-1960 as in Fabien's original codes. *
#'   jour365The day indices computed from the 01-01 of the corresponding year *
#'   rainfall, or whatever name inputed : the daily rainfall data given as an
#'   input * REW: The computed Relative Extractable Water for trees accross the
#'   soil profile (weighted by tree roots distribution accross the profile), for
#'   each simulated day.
#'
#' @details Here are a few notes to the attention of future users of this
#'   package: * The original Soil Water Model is from Fabien Wagner (put
#'   appropriate ref) and is exactly the code he used to provide REW and such
#'   outputs up to now. This tool is meant to simplify the process of obtaining
#'   such outputs, while letting open doors in order to allow some
#'   parameter-tuning in case a few users would be tempted to explore it. *
#'   Parameter-tuning must be done with caution. I would advise not to change
#'   anything except rainfall data, and keep using this package on Paracou only,
#'   because that is what Fabien advised us to do. * I noted several differences
#'   between the algorithm, parameters and equations decribed in the article, and
#'   those the C code called in this function actually does and uses. These differences will be documented as far as I can and will probably be available upon demand to Geraldine Derroire. I think this is
#'
#' @export
#'
#' @examples
#' @md
run_original_SWM <- function(rainfall_data = NULL,
                             rainfall_col = NA,
                             date_col = NA,
                             date_format = NA,
                             drainage = F,
                             ...){
# rm(list = ls())


  # Load default arguments and dll ------------------------------------------


  Em<-0.64
  Rm<-8.98
  # I0 <- 592.1
  if(isFALSE(drainage)){
    file <- "lib/SWMoutput2013.dll"
  }
  else{
    file <- "lib/SWMoutput2013_avec_drainage.dll"
  }

  dyn.load(system.file(file, package="SWMviaOriginalCode"))

  data("default_arguments")
  # data(m3)
  # data("default_arguments")
  data("interception")
  # dyn.unload("inst/lib/SWMoutput2013.dll")

  if(is.null(rainfall_data)){

    data(stoc)
    rainfall_data = stoc
    rainfall_col = "pluie"
    date_col = "date"
    date_format = "dmy"
    rainfall_data$date = paste(rainfall_data$day,
                               rainfall_data$month,
                               rainfall_data$year,
                               sep ="_")

  }
  else{
    if(is.na(rainfall_col)){
      if("pluie" %in% names(rainfall_data)){
        rainfall_col <- "pluie"
      }
      else stop("please indicate which column contains rainfall values with argument rainfall_col")
    }
    else if(!rainfall_col %in% names(rainfall_data)) stop("please indicate which column contains rainfall values with argument rainfall_col")
    if(is.na(date_col)){
      if(all(c("month","day","year") %in% names(rainfall_data))){
        date_col = "date"
        date_format = "dmy"
        rainfall_data$date = paste(rainfall_data$day,
                                   rainfall_data$month,
                                   rainfall_data$year,
                                   sep ="_")
      }
      else stop("Please either specify the name of the column containing dates corresponding to your rainfall data, or make sure that there are three column named 'day', 'month' and 'year' containing the corresponding information, in order to reconstitute and parse the dates.")
    }
    else if(!date_col %in% names(rainfall_data)){
      stop("The name you specified for date_col does not correspond to any dataset's columns")
    }
  }
  # print("here")
  default_arguments[["Pluie"]] <- rainfall_data[,rainfall_col]

  default_arguments[["N_day"]] <- nrow(rainfall_data)
  default_arguments[["ObsDay"]] <- rep(0,nrow(rainfall_data))
  # default_arguments[["ObsDay"]][2800] <- 1 # For compatibility with original code ?
  default_arguments[["ETP"]] <- rep(3.970279,nrow(rainfall_data))
  default_arguments[["REW"]] <- rep(0,nrow(rainfall_data))


# Extract optional arguments from ellipsis if there are any ---------------

  .dots <- list(...)
  # print(as.character(substitute(list(...)))[-1L])

  if(length(.dots) > 0){

    .dotnames <- names(list(...))
    if(is.null(.dotnames)){
      .dotnames <- as.character(substitute(list(...)))[-1L]
    }
    else if(any(.dotnames == "")){
      .dotnames[.dotnames == ""] <- as.character(substitute(list(...)))[-1L][.dotnames == ""]
    }
    # print(.dotnames)
    names(.dots) <- .dotnames
    #
    for(n in .dotnames){

      switch(n,
             "I0" = {
               temp <- .dots[[n]]
               if(temp > 0){
                 default_arguments[["I0"]] <- temp
               }
             },
             "interception_parameters" = {
               #check the object given as an input and fill the blanks if there are any
               temp <- try(check_format_interception_parameters(.dots[[n]]))
               #then attribute new parameters
               if(!inherits(temp, "try-error")){
                 default_arguments[["p"]][9] <- temp[["S"]]
                 E_m <- temp[["E_m"]]
                 R_m <- temp[["R_m"]]
                 default_arguments[["p"]][10] <- temp[["p_t"]]
                 default_arguments[["p"]][11] <- temp[["cc"]]
               }
               else warning("Given an error occurring while checking and formating the interception_parameters you provided, these will be let to their default.")
             },
             "PAI" = {
               temp <- .dots[[n]]
               if(length(temp) == 1 & temp > 0){
                 default_arguments[["LAI"]] <- temp
               }
               else warning("Given an error related to wrong format of PAI, this is let to its default.")
             },
             "understorey_parameters" = {
               temp <- try(check_format_understorey_parameters(.dots[[n]]))
               if(!inherits(temp, "try-error")){
                 default_arguments[["p"]][5] <- temp[["k"]]
                 default_arguments[["p"]][6] <- temp[["FractG"]]
                 default_arguments[["p"]][7] <- temp[["a"]]
                 default_arguments[["p"]][8] <- temp[["lambdaEU"]]
               }
               else warning("Given an error occurring while checking and formating the understorey_parameters you provided, these will be let to their default.")
             },
             "soil_data" = {
               temp <- try(check_format_soil_data(.dots[[which(.dotnames ==n)]]))
               if(!inherits(temp, "try-error")){
                 default_arguments[["ThetaPWP"]] <- temp$ThetaPWP
                 # print(default_arguments[["ThetaPWP"]])
                 default_arguments[["ThetaFC"]] <- temp$ThetaFC
                 # print(default_arguments[["ThetaFC"]])
                 default_arguments[["L"]] <- nrow(temp)
                 default_arguments[["ProfMax"]] <- sort(temp$layer_depth, decreasing = FALSE)
                 default_arguments[["ProfObs"]] <- rep(1, nrow(temp))
                 default_arguments[["EW_obs"]] <- rep(0.2, nrow(temp))
                 default_arguments[["EW_calc"]] <- rep(0, nrow(temp))
                 delta <- temp$layer_depth[nrow(temp)] - temp$layer_depth[nrow(temp)-1]
                 default_arguments[["N_layer"]] <- length(-1:-(temp$layer_depth[nrow(temp)]+delta/2))
                 default_arguments[["roots_couche"]] <- -1:-(temp$layer_depth[nrow(temp)]+delta/2)
                 # print(delta/2)
               }
               else warning("Given an error occurring while checking and formating the soil_data you provided, corresponding parameters will be let to their default.")
             },
             "transpiration_parameters" = {
               temp <- try(check_format_transpiration_parameters(.dots[[n]]))
               if(!inherits(temp, "try-error")){
                 default_arguments[["ThTr"]] <- temp[["ThetaTr"]]
                 default_arguments[["p"]][3] <- temp[["Threshold"]]

               }
               else warning("Given an error occurring while checking and formating the understorey_parameters you provided, these will be let to their default.")
             },
             "roots_data" = {
               temp <- try(check_format_roots_data(.dots[[n]]))
               if(!inherits(temp, "try-error")){
                 # if(temp$N_layers = )
                 default_arguments[["ThExp"]] <- temp[["lambda_roots"]]
                 # default_arguments[["p"]][3] <- temp[["Threshold"]]

               }
               else warning("Given an error occurring while checking and formating the understorey_parameters you provided, these will be let to their default.")
             },
             "ETP" = {
               temp <- .dots[[n]]
               if(length(temp) == 1 & temp > 0){
                 default_arguments[["ETP"]] <- rep(temp,N_day)
               }
               else if(length(temp) == N_day & all(temp > 0)){
                 default_arguments[["ETP"]] <- temp
               }
               else warning("Given an error related to wrong format of PAI, this is let to its default.")
             },
             "p4" = {
               temp <- .dots[[n]]
               if(temp == 0){
                 default_arguments[["p"]][4] <- 0
               }
             },
             warning(paste0("Unrecognised additional parameter ",n," has been ignored. Please check the documentation's details about optional parameter tuning to properly specify your modifications."))
      )
    }
  }

# Run the SWMoutput2013 function ------------------------------------------
  default_arguments[["In"]]<-interception( default_arguments[["Pluie"]], Em, Rm, default_arguments[["p"]][9], default_arguments[["p"]][10], default_arguments[["p"]][11])
  # res1=SWMoutput(ThExp,ThetaPWP,ThetaFC, ThTr, N_layer, Pluie, EW_obs, jour, ProfMax, In, roots_couche, p, I0,LAI, ETP,LogL, N_day, ObsDay, L,ProfObs,Lobs, REW,percREW, EW_calc)

  if(isTRUE(drainage)){
    default_arguments[["Dr"]]=rep(0,N_day)
    function_to_call <- SWMoutput_drainage
  }
  else function_to_call <- SWMoutput


  res <- do.call(function_to_call, default_arguments)



# Extract the outputs we are interested in --------------------------------

  rainfall_data$REW=res[[22]]
  # rainfall_data$date <- do.call(getExportedValue("lubridate",date_format),list(rainfall_data$date))
  # rainfall_data$SWD=rainfall_data$REW
  # rainfall_data$SWD[rainfall_data$SWD>0.3]=0
  # rainfall_data$SWD[rainfall_data$SWD!=0]=0.3-rainfall_data$SWD[rainfall_data$SWD!=0]

  rainfall_data$date <- do.call(getExportedValue("lubridate",date_format),list(rainfall_data$date))

  if(!all(c("year","month","day") %in% names(rainfall_data))){
    rainfall_data$year <- lubridate::year(rainfall_data$date)
    rainfall_data$month <- lubridate::month(rainfall_data$date)
    rainfall_data$day <- lubridate::day(rainfall_data$date)
  }

  rainfall_data$julian_day <- as.integer(julian.Date(rainfall_data$date,as.Date("1960-01-01")))
  rainfall_data$jour365 <- as.numeric(format(rainfall_data$date, "%j"))

  class(rainfall_data) <- c("data.frame","SWM")

return(rainfall_data[,c("date","julian_day","day","month","year","jour365",rainfall_col,"REW")])

}


# Internals ---------------------------------------------------------------

#' Title
#'
#' @param ThExp Single numeric. The exponential regression parameter used to
#'   model fine roots density along the soil profile. Can be obtained with the
#'   code used in check_format_roots_data
#' @param ThetaPWP Numeric. The median of ThetaPWP posterior distributions
#'   obtained during model calibration, for each sampled depth.
#' @param ThetaFC Numeric. The median of ThetaFC posterior distributions
#'   obtained during model calibration, for each sampled depth.
#' @param ThTr Numeric. The ratio of tree transpiration over PET. Referred to as
#'   "rho" in the article. This ratio is decreased proportionally to REWc (a
#'   measure of water load in a given layer) if it falls under a certain
#'   threshold, that defines stressed hydraulic conditions
#' @param N_layer Sinle integer. The number of 1cm depth layers that are going
#'   to be simulated.
#' @param Pluie Numeric. Rainfall data
#' @param EW_obs Numeric. Same length as real TDR sampled layers. Inputed as all
#'   equal to 0.2. I think it is related to likelihood computation with the
#'   markov chains algorithm, and thus deprecated or to be initialised with
#'   dummy values?
#' @param jour Integer, =2. Used virtually nowhere in the code, not even
#'   appearing in commented lines. Let's pretend it's better to keep it as is.
#' @param ProfMax Integer. The depth of the layers sampled with TDR. It may
#'   correspond to centers rather than lower depth limits in the code that
#'   expands soil parameters to 1cm-layers resolution.
#' @param In Numeric of the same length as Pluie. Interception values computed
#'   for each day assuming one rainfall per day, and using the function
#'   contained in m3.RData (but writted nowhere else in Fabien's folder)
#' @param roots_couche Negative integer. Basically equal to -1:-N_layer. Used in
#'   the computation of understorey roots density.
#' @param p Numeric. A pretty mysterious vector of parameters, of which only a
#'   part is used either in the model, or in the interception function. As for
#'   the arguments of this function, nothing has been written anywhere to allow
#'   external users to decode what it means.
#' @param I0 Single numeric. 592.1, corresponds to mean daily incident radiation
#'   to the canopy.
#' @param LAI Single numeric. Corresponds to PAI in the article, but it would
#'   have been too easy and readable to name thing with at least a bit of
#'   consistency.
#' @param ETP Numeric of the same length as Pluie. It originally contains only
#'   3.970279 but it could take a daily timestep. Better not trying that.
#' @param LogL Zero. Perhaps log likelyhood, used in MCMC but useless here.
#' @param N_day Single integer. The number of days that are simulated. Must be
#'   equal to the length of Pluie
#' @param ObsDay Same length as Pluie. Originally a vector of zeros containing a
#'   single 1 at position 2800. Probably used to compute likelyhood.
#' @param L Single integer. The number of layers actually measured with TDR,
#'   thus the number of values for soil parameters.
#' @param ProfObs Integer. Originally a vector of 1's of length 3. I genuinely
#'   ignore what it's for. When it's equal to 0, does something with ThetaFC and
#'   ThetaPWP values, but... Only Fabien could perhaps remember these kind of
#'   very subtle details.
#' @param Lobs Single integer. The lines where it used to be implied have been
#'   turned to comments. It seems that it's a deprecated argument replaced by L.
#'   Perhaps it was used for calibrations, because it still is in the MCMCoutput
#'   function written in C. Again, Fabien might be able to tell us more about
#'   that than I'd ever be able to.
#' @param REW A vector of 0's of the same length as Pluie. I guess it's given as
#'   an input to simplify dynamic memory allocation ? But I believe this could
#'   be inclueded in the C code since this information is contained in other
#'   arguments. Nothing to be modified here except the length of this arg.
#' @param percREW Zero... Same remark here. Why isn't its declaration and
#'   instance into the C code?? Also not to be modified.
#' @param EW_calc Idem.
#' @param Dr Idem
#'
#' @return
#' @export
#'
#' @examples
SWMoutput_drainage <-function(ThExp,
                              ThetaPWP,
                              ThetaFC,
                              ThTr,
                              N_layer,
                              Pluie,
                              EW_obs,
                              jour,
                              ProfMax,
                              In,
                              roots_couche,
                              p,
                              I0,
                              LAI,
                              ETP,
                              LogL,
                              N_day,
                              ObsDay,
                              L,
                              ProfObs,
                              Lobs,
                              REW,
                              percREW,
                              EW_calc,
                              Dr){

  .C("SWMoutput",
     as.double(ThExp),
     as.double(ThetaPWP),
     as.double(ThetaFC),
     as.double(ThTr),
     as.integer(N_layer),
     as.double(Pluie),
     as.double(EW_obs),
     as.integer(jour),
     as.integer(ProfMax),
     as.double(In),
     as.double(roots_couche),
     as.double(p),
     as.double(I0),
     as.double(LAI),
     as.double(ETP),
     as.double(LogL),
     as.integer(N_day),
     as.double(ObsDay),
     as.integer(L),
     as.integer(ProfObs),
     as.integer(Lobs),
     as.double(REW),
     as.double(percREW),
     as.double(EW_calc),
     as.double(Dr),
     PACKAGE = "SWMoutput2013_avec_drainage")
}

SWMoutput <- function (ThExp,
                       ThetaPWP,
                       ThetaFC,
                       ThTr,
                       N_layer,
                       Pluie,
                       EW_obs,
                       jour,
                       ProfMax,
                       In,
                       roots_couche,
                       p,
                       I0,
                       LAI,
                       ETP,
                       LogL,
                       N_day,
                       ObsDay,
                       L,
                       ProfObs,
                       Lobs,
                       REW,
                       percREW,
                       EW_calc) {
  .C("SWMoutput",
     as.double(ThExp),
     as.double(ThetaPWP),
     as.double(ThetaFC),
     as.double(ThTr),
     as.integer(N_layer),
     as.double(Pluie),
     as.double(EW_obs),
     as.integer(jour),
     as.integer(ProfMax),
     as.double(In),
     as.double(roots_couche),
     as.double(p),
     as.double(I0),
     as.double(LAI),
     as.double(ETP),
     as.double(LogL),
     as.integer(N_day),
     as.double(ObsDay),
     as.integer(L),
     as.integer(ProfObs),
     as.integer(Lobs),
     as.double(REW),
     as.double(percREW),
     as.double(EW_calc),
     PACKAGE = "SWMoutput2013")
}
