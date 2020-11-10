#' Format soil data for Soil Water Balance Modelling
#'
#' This function takes soil FC and PWP (measured or estimated for each layer) as an input, checks the values and returns the corresponding values at a 1cm layer-depth resolution (assuming valeus are homogenous within a measured layer)
#'
#' @param soil_data data.frame, containing one-line-per-layer PWP and FC
#' @param layer_col character, the name of the column containing TDR measurement depths
#' @param FC_col character, the name of the column containing FC
#' @param PWP_col character, the name of the column containing PWP
#'
#' @return
#' @export
#'
#' @examples
check_format_soil_data <- function(soil_data,
                                   layer_col = "layer_depth",
                                   FC_col = "ThetaFC",
                                   PWP_col = "ThetaPWP"){
  if(!is.data.frame(soil_data))
    stop("the input data must be a data.frame object as described in the documentation")
  if(any(is.na(soil_data)))
    stop("NA values are not allowed in the input dataset.")
  if(any(!soil_data$init > soil_data$end))
    stop("Something in your depth layer delimitations is going wrong")
  # ThetaPWP must be Higher than ThetaPWP for a given layer layer
  if(any(!soil_data$ThetaPWP<soil_data$ThetaFC))
    stop("ThetaPWP must be smaller than ThetaFC for a given layer")

  # print(soil_data)

  # soil_data$med=ceiling((soil_data$init+soil_data$end )/2)

  soil_data <- soil_data[order(soil_data$layer_depth),]


  return(soil_data)
}
