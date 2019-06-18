#' Pseudocode version of the oncoming update for SWBM
#'
#' @param data
#' @param rainfall_col
#' @param rainfall_format
#' @param date_col
#' @param date_format
#' @param pai_col
#'
#' @return
#' @export
#'
#' @examples
SWB_model <- function(climate_data,
                      soil_data,
                      rainfall_col = "pre",
                      rainfall_format = "daily",
                      date_col = "date",
                      date_format = "ymd",
                      pai_col = "pai",
                      layer_limits_cols = c("depth_layer_init","depth_layer_end"),
                      root_dist_col = "percentRoots",
                      theta_pwp_col = "ThetaPWP",
                      theta_FC_col = "ThetaFC",
                      ...){

  if(!is.data.frame(data))
    stop("The input dataset must be a data.frame containing ...")

  data <- check_rename_variable_col(rainfall_col, "rainfall",data)
  data <- check_rename_variable_col(date_col, "date",data)
  data <- check_rename_variable_col(pai_col, "pai",data)

  if(rainfall_format != "daily"){
    data <- monthly_to_daily(data)
  }



# Model parameters --------------------------------------------------------

#Interception
S <- 1.5
E_m <- 0.64
R_m <- 8.98
p_t <- 0.036
cc <- 0.99




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

               }
             })
    }
  }

  # Model structure

  # Loop on days



  # Formatting output
}


