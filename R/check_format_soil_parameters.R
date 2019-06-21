#' Format soil data for Soil Water Balance Modelling
#'
#' This function takes soil FC and PWP (measured or estimated for each layer) as an input, checks the values and returns the corresponding values at a 1cm layer-depth resolution (assuming valeus are homogenous within a measured layer)
#'
#' @param soil_data data.frame, containing one-line-per-layer PWP and FC
#' @inheritParams  check_format_roots_data
#' @param FC_col character, the name of the column containing FC
#' @param PWP_colcharacter, the name of the column containing PWP
#'
#' @return
#' @export
#'
#' @examples
check_format_soil_data <- function(soil_data,
                                   layer_init_col,
                                   layer_end_col,
                                   FC_col,
                                   PWP_col){
  if(!is.data.frame(soil_data))
    stop("the input data must be a data.frame object as described in the documentation")
  if(any(is.na(soil_data)))
    stop("NA values are not allowed in the input dataset.")
  if(any(!soil_data$init > soil_data$end))
    stop("Something in your depth layer delimitations is going wrong")
  # ThetaPWP must be Higher than ThetaPWP for a given layer layer
  if(any(ParamSoil$ThetaPWP<ParamSoil$ThetaFC))
    stop("ThetaPWP must be smaller than ThetaFC for a given layer")

  soil_data <- check_rename_variable_col(layer_init_col, "init", soil_data)
  soil_data <- check_rename_variable_col(layer_end_col, "end", soil_data)
  soil_data <- check_rename_variable_col(FC_col, "FC", soil_data)
  soil_data <- check_rename_variable_col(PWP_col, "PWP", soil_data)



  # soil_data$med=ceiling((soil_data$init+soil_data$end )/2)
  N_sampled <- nrow(soil_data$init)
  N_modelled <- length((min(soil_data$init)+1):max(soil_data$end))
  soil_model <- data.frame(init = rep(NA,N_modelled), end = rep(NA,N_modelled), depth = rep(NA,N_modelled), FC = rep(NA,N_modelled), PWP = rep(NA,N_modelled))
  soil_data <- soil_data[order(soil_data$init),]
  l = min(soil_data$init)+1
  for(i in N_sampled){
    layers_temp <- (soil_data$init[i]+1):soil_data$end

    soil_model$init[l:l+(length(layers_temp)-1)] <- soil_data$init[i]
    soil_model$end[l:l+(length(layers_temp)-1)] <- soil_data$end[i]
    soil_model$FC[l:l+(length(layers_temp)-1)] <- soil_data$FC[i]
    soil_model$PWP[l:l+(length(layers_temp)-1)] <- soil_data$PWP[i]

    l = l+(length(layers_temp)-1)
  }

  return(list(soil_parameters = soil_model,N_layers = nrow(soil_model)))
}
