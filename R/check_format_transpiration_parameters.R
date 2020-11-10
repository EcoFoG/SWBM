#'Check and format understorey parameters for use in the Soil Water Balance
#'model
#'
#'This function is initially meant for internal use.
#'
#' @inheritParams check_format_interception_parameters
#'
#' @details The parameters are inputed either separately or groupedly.
#'
#'  If inputed separately, each argument must have a name that corresponds to
#'  one of the Interception model's parameters. If the parameters are grouped
#'  into a list or a data.frame, the names of the columns/elements must also
#'  fulfill this requirement. Also note that if any parameter is missing, its
#'  corresponding default value will be used. Finally, if any argument/element
#'  has a name that does not correspond to any of the hereafter listed
#'  parameters, said argument will be ignored.
#'
#' @return A named list containing the Understorey submodel parameters in this order: k, FractG, a, lambdaEU.
#' @export
#'
#' @examples
check_format_transpiration_parameters <- function(...){



  # Extract the ellipsis and set the default values of the interception parameters
  .dots <- list(...)
  ThetaTrt <- 0.9971838
  Thresholdt <- 0.4


  # Test the input type (isolated scalars, data.frame, list)


  if(length(.dots) == 0){
    # print(names(.dots))
    message("returning default understorey parameters since nothing has been given as an input...")

    return(list(ThetaTr = ThetaTrt, Threshold = Thresholdt))
  }
  if(length(.dots) == 1){
    dot <- .dots[[1]]
    if(is.data.frame(dot)){
      .dots <- as.list.data.frame(dot)
    }
    else if(is.list(dot)){
      .dots <- dot
    }
  }

  for(n in names(.dots)){

    temp <- .dots[[n]]

    if(is.data.frame(temp | is.list(temp))){
      stop("You must input either scalars corresponding to the parameters you want to modifie, OR a stacked form (either a correctly named list, or a data.frame with a single row), but NOT a mix of both.")
    }
    if(!length(temp) == 1)
      stop("Please provide only one value per parameter")

    switch(n,
           "ThetaTr" = {
             if(!is.numeric(temp)) stop("k must be numeric")
             # if(temp<0) stop("negative values are not allowed for k")
             Thetatrt <- temp
           },
           "Threshold" = {
             if(!is.numeric(temp)) stop("FractG must be numeric")
             # if(temp<0 | temp>1) stop("FractG must be in [0:100]")
             Thresholdt <- temp
           },
           message(paste0(n," is not the name of any of the model parameters. It has been ignored."))
    )
  }

  return(list(ThetaTr = ThetaTrt, Threshold = Thresholdt))
}


