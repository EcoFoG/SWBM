#' Prepare the root parameters for use un the Soil Water Balance model
#'
#' This function takes sampled root proportion among layers as an input dataset,
#' and uses a negative exponential function (/code{Rfd_{l} = /lambda
#' e^{-lambda*l}}) to estimate the distribution of fine roots among the soil, at
#' a 1cm resolution.
#'
#' @param roots_data data.frame, containing one-line-per-layer fine roots percentages
#'   percentage as well as upper and lower depth bounds of the sampled layers (always positive numbers: low values being close to the surface, and high value being deep)
#' @param layer_init_col character, the name of the column containing upper (close to the surface) end depth for each layer
#' @param layer_end_col  character, the name of the column containing lower (far from the surface) end depth for each layer
#' @param percent_col character, the name of the column containing observed fine roots percentage for each layer (percents, i.e. the sum of the column is 1)
#'
#' @return a list containing two elements, the first a data.frame containing modelled fine roots density for each 1cm sublayer, as well as information on
#'
#' @export
#'
#' @examples
check_format_roots_data <- function(roots_data,
                                    layer_init_col = "depth_layer_init",
                                    layer_end_col = "depth_layer_end",
                                    percent_col = "percentRoots"){
  if(!is.data.frame(roots_data))
    stop("the input data must be a data.frame object as described in the documentation")
  if(any(is.na(roots_data)))
    stop("NA values are not allowed in the input dataset.")

  roots_data <- check_rename_variable_col(layer_init_col, "init", roots_data)
  roots_data <- check_rename_variable_col(layer_end_col, "end", roots_data)
  roots_data <- check_rename_variable_col(percent_col, "percent", roots_data)

  if(any(roots_data$init > roots_data$end))
    stop("Something in your depth layer delimitations is going wrong")

  # roots_data$med=ceiling((roots_data$init+roots_data$end )/2)
  N_sampled <- nrow(roots_data)
  N_modelled <- length((min(roots_data$init)+1):max(roots_data$end))

  roots_model <- data.frame(init = rep(NA,N_modelled), end = rep(NA,N_modelled), depth = 1:N_modelled, percent = rep(NA,N_modelled))
  roots_data <- roots_data[order(roots_data$init),]
  # print(roots_data)

  for(i in 1:N_sampled){
    init_temp <- roots_data$init[i]+1
    end_temp <- roots_data$end[i]
    # ifelse(i == N_sampled, roots_data$end[i], roots_data$end[i]-1)
    layers_temp <- init_temp:end_temp
    # print(layers_temp)
    roots_model$init[layers_temp] <- roots_data$init[i]
    roots_model$end[layers_temp] <- roots_data$end[i]
    roots_model$depth[layers_temp] <- layers_temp
    roots_model$percent[layers_temp] <- roots_data$percent[i]/length(layers_temp)
  }

  model_rfd <- function(p)
  {
    percent_temp=p*exp(-p*roots_model$depth)
    squared_error=sum((percent_temp-roots_model$percent)^2)
    return(squared_error)
  }

  lambda_roots <- as.numeric(optimize(model_rfd, c(0, 1))[1])

  roots_model$density <- lambda_roots*exp(-lambda_roots*roots_model$depth)
  roots_model$density <- roots_model$density/sum(roots_model$density)

  return( list(lambda_roots = lambda_roots, N_layer = nrow(roots_model)))
  # return(list(root_parameters = roots_model, lambda_roots = lambda_roots))
}
