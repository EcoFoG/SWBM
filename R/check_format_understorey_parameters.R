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
check_format_understorey_parameters <- function(...){



  # Extract the ellipsis and set the default values of the interception parameters
  .dots <- list(...)
  kt <- 0.88
  FractGt <- 0.2
  at <- 0.010
  lambdaEUt <-0.1


  # Test the input type (isolated scalars, data.frame, list)

  dotnames <- as.character(substitute(list(...)))[-1L]
  if(length(.dots) == 0){
    message("returning default understorey parameters since nothing has been given as an input...")
    return(list(k = kt,FractG = FractGt,a = at,lambdaEU = lambdaEUt))
  }
  if(length(.dots) == 1){
    dot <- .dots[[1]]
    if(is.data.frame(dot)){
      .dots <- as.list.data.frame(dot)
      dotnames <- names(dot)
    }
    else if(is.list(dot)){
      .dots <- dot
      dotnames <- names(dot)
    }
  }

  for(n in dotnames){

    temp <- .dots[[n]]

    if(is.data.frame(temp) | is.list(temp)){
      stop("You must input either scalars corresponding to the parameters you want to modifie, OR a stacked form (either a correctly named list, or a data.frame with a single row), but NOT a mix of both.")
    }
    if(!length(temp) == 1)
      stop("Please provide only one value per parameter")

    switch(n,
           "k" = {
             if(!is.numeric(temp)) stop("k must be numeric")
             if(temp<0) stop("negative values are not allowed for k")
             kt <- temp
           },
           "FractG" = {
             if(!is.numeric(temp)) stop("FractG must be numeric")
             if(temp<0 | temp>1) stop("FractG must be in [0:100]")
             FractGt <- temp
           },
           "a" = {
             if(!is.numeric(temp)) stop("a must be numeric")
             if(temp<0 | temp>1) stop("a must be in [0;100]")
             at <- temp
           },
           "lambdaEU" = {
             if(!is.numeric(temp)) stop("lambdaEU must be numeric")
             # if(temp<0 | temp>1) stop("lambdaEU (proportion of rain diverted to the trunk) must be in [0;1]")
             lambdaEUt <- temp
           },
           message(paste0(n," is not the name of any of the model parameters. It has been ignored."))
    )
  }

  return(list(k = kt,FractG = FractGt,a = at,lambdaEU = lambdaEUt))
}



