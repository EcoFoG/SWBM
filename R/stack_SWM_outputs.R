#' Stack SWM outputs
#'
#' @param ... any objects outputed from run_original_SWM or stack_SWM
#' @param SWM_names Multiple character of same length as ... The names you want to attribute to each of the aforementioned object
#' @param time_correspondance Logical, do dates have to match perfectly ?
#'
#' @return A stack.
#' @export

stack_SWM <- function(...,
                      SWM_names = NULL,
                      time_correspondance = TRUE){
  .dots <- list(...)
  if(is.null(names(.dots))){
    .dotnames <- as.character(substitute(list(...)))[-1L]
  }
  else{
    .dotnames <- names(.dots)
    if(any(names(.dots) == "")){
      .dotnames[names(.dots) == ""] <- as.character(substitute(list(...)))[-1L][names(.dots) == ""]
    }
  }

  names(.dots) <- .dotnames

  class.dots <- rep(NA, length(.dots))

  for(i in 1:length(class.dots)){
    class.dots[i] <- ifelse(inherits(.dots[[i]], "SWM"),
                            "SWM",
                            ifelse(inherits(.dots[[i]],"SWM_stack"),
                                   "SWM_stack",
                                   class(.dots[[i]])))
  }



  if(!all(class.dots == "SWM_stack" | class.dots == "SWM"))
    stop("stack_SWM is designed to stack single SWM outputs or SWM stacks outputted by this same function")


  if(all(class.dots == "SWM_stack")){
    return(do.call(rbind, .dots))
  }
  else if(any(class.dots == "SWM_stack")){
    already_stacked <- which(class.dots == "SWM_stack")
    # already_stacked <- .dots[class.dots == "SWM_stack"]
    to_stack <- which(class.dots == "SWM")
    # to_stack <- .dots[class.dots == "SWM"]
  }
  else{
    to_stack <- 1:length(.dots)
    # to_stack <- .dots
    already_stacked <- NULL
  }
  if(is.null(SWM_names)){
    SWM_names <- .dotnames
  }

  if(isTRUE(time_correspondance)){
    for(s in to_stack[-1]){
      ts <- .dots[[s]]
      # print(ts$date)
      # if(!identical(ts$date , to_stack[[1]]$date))
      if(!identical(ts$date , .dots[[1]]$date))
        stop("You specified 'time_correspondance = TRUE' but dates do not correspond.")
    }
  }

  # stack <- do.call(rbind, lapply(1:length(names(to_stack)), function(n){
  stack <- do.call(rbind, lapply(to_stack, function(n){
    tempt <- .dots[[n]]
    # tempt <- .dots[[n]]
    # tempt <- tempt$daily_aggregated_outputs
    # print(tempt)
    tempt$SWM_name <- SWM_names[n]
    return(tempt)
  }))

  if(!length(already_stacked)==0){
    if(length(already_stacked) == 1){
      # print(class(already_stacked[[1]]))
      # class(already_stacked[[1]]) <- "data.frame"
      # print(head(rbind(stack, already_stacked[[1]])))
      stack <- rbind(stack, .dots[[already_stacked]])
    }
    else{
      stack <- rbind(stack, do.call(rbind,lapply(already_stacked, function(n){
        tempt <- already_stacked[[n]]
        # print(class(tempt))
        if(any(is.null(unique(tempt$SWM_name)))| any(is.na(unique(tempt$SWM_name)))){
          tempt$SWM_name <- SWM_names[n]
        }
        return(tempt)
      })))
    }

  }


  class(stack) <- c("data.frame","SWM_stack")
  return(stack)

}

