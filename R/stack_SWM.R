stack_SWM <- function(...,
                      SWM_names = NULL,
                      time_correspondance = TRUE){
  .dots <- list(...)
  .dotnames <- as.character(substitute(list(...)))[-1L]
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


  if(any(class.dots == "SWM_stack")){
    already_stacked <- .dots[class.dots == "SWM_stack"]
    to_stack <- .dots[class.dots == "SWM"]
  }
  else{
    to_stack <- .dots
    already_stacked <- NULL
  }
  if(is.null(SWM_names)){
    SWM_names <- .dotnames
  }
  if(isTRUE(time_correspondance)){
    for(s in to_stack[-1]){
      if(!identical(s$daily_aggregated_outputs$date , to_stack[[1]]$daily_aggregated_outputs$date))
        stop("You specified 'time_correspondance = TRUE' but dates do not correspond.")
    }
  }

  stack <- do.call(rbind, lapply(1:length(names(to_stack)), function(n){
    tempt <- .dots[[n]]
    tempt <- tempt$daily_aggregated_outputs
    # print(tempt)
    tempt$SWM_name <- SWM_names[n]
    return(tempt)
  }))

  if(!is.null(already_stacked)){
    if(length(already_stacked) == 1){
      print(class(already_stacked[[1]]))
      class(already_stacked[[1]]) <- "data.frame"
      print(head(rbind(stack, already_stacked[[1]])))
      stack <- rbind(stack, already_stacked[[1]])
    }
    else{
      stack <- rbind(stack, do.call(rbind,lapply(names(already_stacked), function(n){
        tempt <- already_stacked[[n]]
        print(class(tempt))
        if(any(is.null(unique(tempt$SWM_name)))| any(is.na(unique(tempt$SWM_name)))){
          tempt$SWM_name <- n
        }
        return(tempt)
      })))
    }

  }


class(stack) <- c("SWM_stack","data.frame")
return(stack)

}
#   test3 <- list(test,test2)
# test3[[1]]$daily_aggregated_outputs
#
#
# a <- list(b = 1, c = "1", d = 5)


#
# foo <- function(...){
#   nam = c()
#   dots <- list(...)
#   if("a" %in% names(dots)){
#     print("ok")
#   }
#   print(names(dots))
#   for(i in 1:length(dots))
#     print(...elt(n = i))
#     # print(dots[[i]])
#     nam <- c(nam, names(dots[[i]]))
#   return(as.character(substitute(list(...)))[-1L])
#     # return(as.list(substitute(list(...))))
#     # return(as.list(substitute(list(...))))
# }
# foo( a,a$b,  a$c)
