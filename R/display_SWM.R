display_SWM <- function(model_output,
                        what = "REW_global",
                        type = "line",
                        save_graph = F,
                        path_save,
                        filename,
                        device,
                        ...){
  if(length(what) > 1){
    if(all(what%in%c("REW_global","drainage","restTr"))){
      plot <- .facet_SWM_outputs(model_output = model_output, what = what, type = type, ...)
    }
    else stop("argument what must be one or several of: 'REW_global', 'drainage','resTr'")
  }
  else if(!what%in%c("REW_global","drainage","restTr")){
    stop("argument what must be one or several of: 'REW_global', 'drainage','resTr'")
  }
  else plot <- .display_single_SWM_output(model_output = model_output, what = what, type = type, ...)

  if(save_graph){
    sv <- try(ggplot2::ggsave(file.path(path_save, filename), plot = plot, device = device))
    if(inherits(sv, "try-error")){
      message("The graph could not be saved. Please check the filename and path you specified.")
    }
  }
  return(plot)
}

.facet_SWM_outputs <- function(model_output,
                               what,
                               type,
                               ...){
  .dots <- list(...)
  geom_args <- list()
  title = "Selected SWM outputs"
  subtitle = NULL

  for(n in names(.dots)){
    switch(n,
           "linetype" = {
             geom_args <- c(geom_args, linetype = .dots[[n]])
           },
           "alpha" = {
             geom_args <- c(geom_args, alpha = .dots[[n]])
           },
           "linewidth" = {
             geom_args <- c(geom_args, alpha = .dots[[n]])
           },
           "shape" = {
             geom_args <- c(geom_args, shape = .dots[[n]])
           },
           "colour" = {
             geom_args <- c(geom_args, coulour = .dots[[n]])
           },
           "size" = {
             geom_args <- c(geom_args, size = .dots[[n]])
           },
           "title" = {
             title <- .dots[[n]]
           },
           "subtitle" = {
             subtitle <- .dots[[n]]
           },
           warning(paste0("additional parameter ",n,
                          " is ignored as it does not correspond to any supplementary graphical parameter implemented in this function.")
                   )
           )
  }

  data <- model_output[["daily_aggregated_outputs"]]

  data <- data[,c("date",what)]
  data <- gather(data=data, variable, value, -date)
  # stop("ok")
  print(head(data))
  plot <- ggplot(data = data, mapping = aes(x = date, y = value))+
    do.call(paste0("geom_",type),args = geom_args)+
    facet_wrap(~variable, scales = "free_y")+
    ggtitle(label = title, subtitle = subtitle)
  return(plot)

}
.display_single_SWM_output <- function(model_output,
                               what,
                               type,
                               ...){
  .dots <- list(...)
  geom_args <- list()
  title = paste0("SWM output: ",what)
  subtitle = NULL

  for(n in names(.dots)){
    switch(n,
           "linetype" = {
             geom_args <- c(geom_args, linetype = .dots[[n]])
           },
           "alpha" = {
             geom_args <- c(geom_args, alpha = .dots[[n]])
           },
           "linewidth" = {
             geom_args <- c(geom_args, alpha = .dots[[n]])
           },
           "shape" = {
             geom_args <- c(geom_args, shape = .dots[[n]])
           },
           "colour" = {
             geom_args <- c(geom_args, coulour = .dots[[n]])
           },
           "size" = {
             geom_args <- c(geom_args, size = .dots[[n]])
           },
           "title" = {
             title <- .dots[[n]]
           },
           "subtitle" = {
             subtitle <- .dots[[n]]
           },
           warning(paste0("additional parameter ",n," is ignored as it does not correspond to any supplementary graphical parameter implemented in this function."))
    )
  }

  data <- model_output[["daily_aggregated_outputs"]]

  data <- data[,c("date",what)]
  data <- gather(data=data, variable, value, -date)
  plot <- ggplot(data = data, mapping = aes(x = date, y = value))+
    do.call(paste0("geom_",type),args = geom_args)+
    ggtitle(label = title, subtitle = subtitle)
  return(plot)
}
