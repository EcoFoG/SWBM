#' Run the Soil Water Balance Model from Wagner et al. (2011)
#'
#' This function runs the soil water balance model from Wagner et al. (2011)
#' with default parameters and soil/roots data. The user can input its own
#' dataset for roots sampled distribution and soil parameters (FC and PWP), or
#' tune any of the model parameters by adding it as additional arguments. Only
#' the dates and corresponding rainfall value are mandatory inputs, with
#' corresponding column names and timestep correctly specified.
#'
#' @param climate_data data.frame containing dates or timestamps (numbers with
#'   separators) in a consistent format accross the table, and the corresponding
#'   daily (or monthly, if specified) rainfall values, in milimeters.
#' @param rainfall_col single character, the name of the column containing
#'   rainfall measurements.
#' @param timestep single character, defaults to "daily". If you input rainfall
#'   data with monthly timestep (average), set this argument to "monthly".
#'   Timesteps other than daily yields model outputs that have poorer temporal
#'   resolution, and are not very comparable to the original model outputs
#'   presented in Wagner et. al (2010). Using an "annual" timestep is probably
#'   pointless, yet this possibility  is included in the code.
#' @param date_col single character, the name of the column containing
#'   measurement dates. Dates have to be homogenously formatted.
#' @param date_format single character, one of the following:
#'   "ymd","ydm","dmy","dym","mdy","myd". y:year, m:month, d:day.
#' @param PET_col Optional. single character that defaults to NULL, the name of
#'   the column containing potential evapotranspiration values to be used in the
#'   model. If not specified, a constant, averaged daily light intensity equal
#'   to 586.8 (from Guyaflux data) will be used as in the original model
#'   presented by Wagner et al. (2010). This is recommended if you want to
#'   obtain comparable result, yet the code structure technically allows to use
#'   real measures of daily light intensity.
#' @param light_intensity_col Optional. single character that defaults to NULL,
#'   the name of the column containing daily light intensity values to be used
#'   in the model. If not specified, a constant, averaged daily light intensity
#'   equal to 3.97 (from Guyaflux data) will be used as in the original model
#'   presented by Wagner et al. (2010). This is recommended if you want to
#'   obtain comparable result, yet the code structure technically allows to use
#'   real measures of daily light intensity.
#' @param ... Any additionnal argument corresponding to model parameters, roots
#'   or soil data tables, to replace the model's default parameters. These must
#'   be specificated as presented in \strong{Details} section.
#'
#'
#' @details Here is some additional information about the function's options and
#'   the formatting to input optional arguments that modify default parameters
#'   or datasets. Please read carefully and follow the instructions.
#'
#'   \strong{Inputting different datasets for soil, roots and PAI}
#'
#'   \itemize{
#'     \item PAI data must be inputted using argument name
#'     \code{PAI_data}. The content of this argument can be:
#'       \itemize{
#'         \item A scalar, the PAI value considered constant during the simulated
#'         timeframe
#'         \item A data.frame containing the same dates with the \emph{\strong{same
#'         format}} as those used in the climate dataset, and the corresponding PAI
#'         value.s, with columns named "date" and "PAI" respectively.
#'         \item A list containing the same elements, named "date" and "PAI".
#'       }
#'
#'      Examples:
#'      \code{PAI_data = 6}
#'      \code{PAI_data = data.frame(climate_data$date, 6)}
#'      \code{PAI_data = list(date = climate_data$date, rep(6,nrow(climate_data)))}
#
#'    \item Soil data (field capacity: theta_FC and permanent wilting point: theta_PWP)
#'    must be inputted using argument name \code{soil_data}. The content of this argument
#'    can be:
#'       \itemize{
#'         \item A data.frame containing raw data to be formatted for use in the model,
#'         i.e. to yield FC and PWP values at a 1 cm layer depth resolution. In this case,
#'         the data.frame must contain the upper and lower boundaries of sampled layers,
#'         and the corresponding theta_FC and theta_PWP estimated values with TDR or by
#'         other means, with columns named "depth_layer_init", "depth_layer_end",
#'         "theta_FC" and "theta_PWP", respectively.
#'         \item A list containing the same elements, with the same requirement for names.
#'         \item A data.frame obtained by running \code{\link{check_format_soil_data}}
#'         beforehand on your raw soil data. In this case, please let the default names
#'         of the function's output and it will be automatically detected.
#'       }
#'
#'      Examples:
#'      \code{An example with raw data as a data.frame}
#'      \code{An example with raw data as a list}
#'      \code{An example using check_format_soil_data}
#'
#'
#'    \item roots data (root distribution accross sampled soil layers) must be inputted
#'    using argument name \code{roots_data}. The content of this argument can be:
#'       \itemize{
#'         \item A data.frame containing raw data to be formatted for use in the model,
#'         i.e. to yield fine roots density values at a 1 cm layer depth resolution.
#'         In thiscase, the data.frame must contain the upper and lower boundaries of
#'         sampled layers, and the corresponding root distribution (% of total measured
#'         fine root quantity for all layers), with columns named "depth_layer_init",
#'         "depth_layer_end" and "roots_percent", respectively.
#'         \item A list containing the same elements, with the same requirement for names.
#'         \item A data.frame obtained by running \code{\link{check_format_roots_data}}
#'         beforehand on your raw soil data. In this case, please let the default names
#'         of the function's output and it will be automatically detected.
#'       }
#'
#'      Examples:
#'      \code{An example with raw data as a data.frame}
#'      \code{An example with raw data as a list}
#'      \code{An example using check_format_roots_data}
#'
#'      }
#'
#'   \strong{Tuning any of the SWM parameters with values other than default (\emph{be careful})}
#'
#'   \itemize{
#'     \item Transpiration parameters must be inputed using argument name
#'     \code{transpiration_parameters}.
#'
#'     The names of the parameters and what they correspond to are:
#'     \itemize{
#'     \item \code{rho}: The ratio between transpiration and potential evapotranspiration,
#'     considered constant for LAI over 6 in this framework.
#'     \item \code{threshold}: The threshold for critical value REWc, over which water
#'     availability is supposed to limit tree water uptake in a given soil layer.
#'     }
#'
#'     \code{transpiration_parameters} can be either:
#'       \itemize{
#'         \item A named numeric, that contains one or both of the parameters to be
#'         tuned, e.g. \code{transpiration_parameters = c(rho = 0.8, threshold = 0.4)}.
#'         If one of these parameters is not included in \code{transpiration_parameters},
#'         internal default values are used.
#'         \item A data.frame (1 row) containing the same parameters with appropriate
#'         names.
#'         \item A named list containing the same parameters, with appropriate names.
#'       }
#'
#'    \item Understorey evapotranspiration parameters must be inputed using argument name
#'     \code{understorey_parameters}.
#'
#'     The names of the parameters and what they correspond to are:
#'     \itemize{
#'     \item \code{k}: The Beer-Lambert extinction coefficient used to model light intensity
#'     incident to the understorey.
#'     \item \code{FractG}: The proportion of light incident to the understorey that is lost
#'     by reflection.
#'     \item \code{a}: Understorey evapotranspiration is hypothesized to be proportional to
#'     incident, non-reflected energy (light intensity). a is the proportionality
#'     coefficient between these two variables, in SWM's framework.
#'     \item \code{lambda_EU}: The coefficient of the decreasing exponential used to model
#'     the distribution of water uptake by understorey tree roots across the soil profile.
#'     It is modelled as: \code{proportion(depth) = lambda_EU*exp(-lambdaEU*abs(depth))}
#'     }
#'
#'     \code{understorey_parameters} can be either:
#'       \itemize{
#'         \item A named numeric, that contains one or several of the parameters to be
#'         tuned, e.g. \code{transpiration_parameters = c(k = 0.8, a = 0.4)}.
#'         If one of these parameters is not included in \code{understorey_parameters},
#'         internal default values are used.
#'         \item A data.frame (1 row) containing the same parameters with appropriate
#'         names.
#'         \item A named list containing the same parameters, with appropriate names.
#'       }
#'
#'
#'    \item Interception parameters must be inputed using argument name
#'     \code{interception_parameters}.
#'
#'     The code default uses the original variant of Gash's sparse
#'     rainfall interception model, which is slightly different of
#'     what is described in Wagner et al. (2011). In total, 3
#'     versions of the interception function exist: The original
#'     code's version, the package's V1 version, and the one that
#'     is presented in the article. All are available in the code's
#'     options, and have the same input parameters.
#'
#'     The names of the parameters and what they correspond to are:
#'     \itemize{
#'     \item \code{S}: the canopy compartiment's water retention capacity, or the
#'     amount of rainfall needed to saturate canopy under zero-evaporation conditions.
#'     \item \code{E_m}: the mean evaporation rate from canopies during rainfall.
#'     \item \code{R_m}: the mean rainfall rate (e.g. mm / minute). It is used
#'     to estimate the quantity of water evaporated from the canopy compartiment after
#'     its saturation is reached, and also the quantity of water needed to saturate
#'     canopy, accounting for evaporation before saturation is reached.
#'     \item \code{p_t}: The proportion of rain diverted to the trunk. Depending on the
#'     version of the submodel, this proportion applies either from the moment at which
#'     canopy is saturated, or for all the storm event. This is the principal difference
#'     between the submodel versions, and it has to be explained elsewhere later on.
#'     \item \code{S_t}: The trunks' water retention capacity
#'     \item \code{cc}: The percentage of canopy cover (canopy surface area per ground
#'     area).
#'     }
#'
#'     \code{interception_parameters} can be either:
#'       \itemize{
#'         \item A named numeric, that contains one or several of the parameters to be
#'         tuned, e.g. \code{transpiration_parameters = c(S = 1.8, E_m = 6.4)}.
#'         If one of these parameters is not included in \code{understorey_parameters},
#'         internal default values are used.
#'         \item A data.frame (1 row) containing the same parameters with appropriate
#'         names.
#'         \item A named list containing the same parameters, with appropriate names.
#'       }
#'
#'
#'
#'
#'   Nino out }
#'
#'   Then another sub section
#'
#'
#' @return a data.frame
#' @export
#'
#' @examples
SWB_model <- function(climate_data,
                      rainfall_col = "pre",
                      timestep = "daily",
                      date_col = "date",
                      date_format = "dmy",
                      PET_col = NULL,
                      light_intensity_col = NULL,
                      model = "v1_package",
                      # layer_limits_cols = c("depth_layer_init","depth_layer_end"),
                      # root_dist_col = "percentRoots",
                      # theta_pwp_col = "ThetaPWP",
                      # theta_FC_col = "ThetaFC",
                      ...){



  # data <- check_rename_variable_col(rainfall_col, "rainfall",data)
  # data <- check_rename_variable_col(date_col, "date",data)
  # data <- check_rename_variable_col(pai_col, "pai",data)
  #
  #   if(rainfall_format != "daily"){
  #     data <- monthly_to_daily(data)
  #   }


  # Check and format climat data --------------------------------------------

  climate_data <- check_format_climate_data(climate_data = climate_data,
                                            rainfall_col = rainfall_col,
                                            timestep = timestep,
                                            date_col = date_col,
                                            date_format = date_format,
                                            PET_col = PET_col,
                                            light_intensity_col = light_intensity_col)
  rainfall <- climate_data$rainfall
  PET <- climate_data$PET
  light_intensity <- climate_data$light_intensity



  # Model parameters --------------------------------------------------------

  # Parameters used in the article

  #Interception

  S <- 1.9 # Assumed from Roche (1982)
  E_m <- 0.64 # From Guyaflux data
  R_m <- 8.98 # From Guyaflux data
  S_t <- 0.06 # From Cuartas et al. (2007)
  p_t <- 0.036 # From Cuartas et al. (2007)
  cc <- 0.99 # From Vincent et al. (2010)

  # Climate and PAI


  PET <- 3.97 # From Guyaflux data
  I0 <- 586.8 # From Guyaflux data
  PAI <- 6.92 # From Guyaflux data

  # soil

  data("ParamSoil")
  # soil_data <-
  soil_data <- check_format_soil_data(ParamSoil)
  # n_layers <- nrow(soil_data)
  soil_layers <- soil_data$depth
  theta_FC <- soil_data$FC
  theta_PWP <- soil_data$PWP

  # roots

  data("ParamRoots")
  roots_data <- check_format_roots_data(ParamRoots)
  tree_roots_distribution <- roots_data$density

  # DataRoots = data.frame(depth_layer_init=c(0,30,50,130),depth_layer_end=c(30,50,130,200),precentRoots=c(0.50,0.30,0.15,0.05))

  # understorey
  k <- 0.88 #assumed from Cournac et al. (2007)
  FractG <- 0.01
  a <- 0.1
  lambda_EU <- 0.5

  # transpiration
  rho <- 0.997
  threshold <- 0.4





  # Optional tuning ---------------------------------------------------------


  #Handle the extra arguments # Most parameters are default set in this function
  #but it is meant to be user specifyable, and later on, compatible with
  #parameter estimation or calibration functions.
  args.dots <- list(...)

  if(length(args.dots) != 0){
    for(a in names(args.dots)){
      switch(a,
             "interception_parameters" = {
               #check the object given as an input and fill the blanks if there are any
               temp <- try(check_format_interception_parameters(args.dots(a)))
               #then attribute new parameters
               if(!inherits(temp, "try-error")){
                 S <- temp[["S"]]
                 E_m <- temp[["E_m"]]
                 R_m <- temp[["R_m"]]
                 p_t <- temp[["p_t"]]
                 cc <- temp[["cc"]]
               }
               else warning("Given an error occurring while checking and formating the interception_parameters you provided, these will be let to their default.")
             },
             "data_pai" = {
               temp <- try(check_format_pai(args.dots[[a]]))
               if(!inherits(temp, "try-error")){
                 data$pai <- temp$pai
               }
               else warning("Given an error related to wrong format of data_pai, these values are let to their default.")
             },
             "understorey_parameters" = {
               temp <- try(check_format_understorey_parameters(args.dots[[a]]))
               if(!inherits(temp, "try-error")){
                 k <- temp[["k"]]
                 FractG <- temp[["FractG"]]
                 a <- temp[["a"]]
                 lambaEU <- temp[["lambdaEU"]]
               }
             },
             "transpiration_parameters" = {
               temp = try(check_format_transpiration_parameters(args.dots[[a]]))
               if(!inherits(temp, "try-error")){
                 print("NO")
               }
             })
    }
  }



  #  Model structure and inits ----------------------------------------------

  date <- climate_data$date
  n_days <- length(date)
  n_layers <- nrow(soil_data)
  interception <- compute_interception(rainfall,
                                       S,
                                       E_m,
                                       R_m,
                                       p_t,
                                       cc,
                                       model = model,
                                       S_t = S_t)

  throughfall <- rainfall - interception

  if(any(throughfall < 0)){
    print(throughfall)
    message("A throughfall inferior to 0 has been computed. This is not supposed to exist. Interception cannot be superior to precipitation.")
    message(paste0("N.B.: ", sum(throughfall < 0), "values of interception are higher than their corresponding amount of incident rainfall..."))
    return(ggplot2::ggplot(data = data.frame(t = rep(1:n_days,2), variable = c(rep("interception", n_days), rep("rainfall", n_days)), value = c(interception, rainfall)),
                  mapping = ggplot2::aes(x = t, y = value, colour = variable))+ ggplot2::geom_point()+ ggplot2::geom_vline(xintercept = which(interception > rainfall), colour = "pink",alpha = 0.5))
  }


  positive_throughfall <- (throughfall > 0)

  ## Understorey model

  #Understorey evapotranspiration is linked with understorey water absorption,
  #itself varying amongst layers according to root distribution. No much
  #information being available, absorption is modelled empirically as follows:

  #"We assume the understorey vegetation to absorb water in the top meter of soil
  #with an exponential function of parameter 0.5. "

  #This exponential function is extended until the end depth of the soil samples
  #for which soil parameters have been computed, and the resolution of its
  #discretisation is 1 cm as for other  soil parameters.

  #The 0.5 coefficient applied for the exponential function  in the original paper
  #is named lambda_understorey, parallelically to lambda_roots, letting the door
  #open for further adjustment of this parameter.

  understorey_absorption_distribution <- lambda_EU*exp(-lambda_EU*soil_layers)

  # Nota: I found an inconsistency between the original R code, the C++ code, and
  # the paper. In the original R code, the absorption distribution is then divided
  # by its sum to normalise it to 1 (thus yielding a proportion). This is not even
  # mentioned anywhere in the article. Additionally, this step has been removed in
  # the C++ file stating sth like "new version without normalisation" but without
  # including the reason why such change has been made.


  # See : understorey_absorption_distribution <-
  # understorey_root_distribution/sum(understorey_root_distribution)

  # Furthermore, in the R package parameters lambdaEU = 0.05 whilst it is 0.5 in
  # the article.

  # a <- 0.05*exp(-0.05*1:200)
  # a <- a/sum(a)
  #
  # b <- 0.05*exp(-0.05*1:200)
  # b <- b/sum(b) identical(a,b)
  #

  daily_understorey_and_soil_transpiration <- I0*exp(-k*PAI)*(1-FractG)-a

  daily_understorey_water_absorption_per_layer <- matrix(NA, nrow = n_layers, ncol = n_days)
  for(d in 1:ndays){
    daily_understorey_water_absorption_per_layer[,d] <- daily_understorey_and_soil_transpiration * understorey_absorption_distribution
  }

  ## Extractible water
  EW_max <- theta_FC - theta_PWP
  EW <- matrix(NA, nrow = n_layers, ncol = n_days)
  EW[,1] <- EW_max

  REW_per_layer <- matrix(NA, nrow = n_layers, ncol = n_days)
  #Since EW[,1] (initial extractable water) has been set to its max, REW[,1] is
  #initiallised to 1
  REW_per_layer[,1] <- 1

  REW_global <- rep(0, n_days)
  REW_global[1] <- 1

  REW_critical <- rep(1,n_layers)

  restTr <- rep(0,n_days)

  Drainage <- rep(0, n_days)
  # Core loop on days -------------------------------------------------------

  for( d in 1:n_days){
    # For each layer: is the REW limiting tree water uptake ?
    stressed <- (REW_critical < threshold)
    # add a safety check
    if(any(is.na(stressed))) stop("Error: at least one REWc is NA")

    rho_tmp <- rep(rho, n_layers)
    if(any(stressed)){
      rho_tmp[stressed] <- (REW_critical[stressed]*rho/threshold)
    }

    daily_tree_transpiration_per_layer <- rho_tmp*pet[i-1]*tree_roots_distribution

    print(c(sum(rho_tmp),sum(daily_understorey_water_absorption_per_layer), sum(EW[,d])))

    EW[,i] = EW[,i-1] - (daily_tree_transpiration_per_layer + daily_understorey_water_absorption_per_layer[i-1])

    if(any(EW[,i] < 0)){
      restneg=abs(sum(EW[EW[,i]<0,i]))
      EW[EW[,i]<0,i]=0
    }
    else{
      restneg=0
    }
    restTr[i]=rho*pet[i-1] - sum(rho_tmp* pet[i-1]) + restneg



    water_income <- throughfall[d]

    if(water_income > 0){
      for(l in 1:n_layers){
        EW_max_l <- EW_max[l]
        EW_l_d <- EW[l,d]
        if(EW_l_d < EW_max_l){
          if(water_income < (EW_max_l- EW_l_d)){
            EW[l,d] <- EW_l_d + water_income
            break
          }
          else{
            EW[l,d] <- EW_max_l
            if(l == n_layers){
              Drainage <- water_income - (EW_max_l- EW_l_d)
              break
            }
            else{
              water_income <- water_income - (EW_max_l- EW_l_d)
            }
          }
        }
        else next
      }
    }

    REW_critical <- EW[,i] / EW_max
    REW_per_layer[,i] <- REW_critical*tree_roots_distribution
    REW_global[i] <- sum(REW_per_layer)

  }



  # Formatting output
  daily_aggregated_outputs <- data.frame(date = date, REW_global = REW_global, drainage = Drainage, restTr = restTr)
  REW_per_layer <- as.data.frame.matrix(REW_per_layer)
  EW <- as.data.frame.matrix(EW)
  row.names(REW_per_layer) <- row.names(EW) <- 1:n_layers
  names(REW_per_layer) <- names(EW) <- as.character(date)

  SWM_output <- list(daily_aggregated_outputs = daily_aggregated_outputs,
                     REW_per_layer = REW_per_layer,
                     EW = EW)
  class(SWM_output) <- "SWM"
  return(SWM_output)
}


