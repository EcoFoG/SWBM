
# Functions to display single sims ----------------------------------------


#' Display a single SWM simulation's output
#'
#' This function is meant to help the non-advanced R user to display the SWM
#' outputs in a nice way, using ggplot2. Graphical parameters can be further
#' tuned if specified in the optional arguments.
#'
#' @param model_output data.frame and SWM object. The output of
#'   Srun_original_SWM.
#' @param what The name of the variable to display, among "REW" and "Drainage"
#'   (if relevant)
#' @param type The type of graph. It corresponds to a geom_ in ggplot's
#'   namespace, e.g. "line" for geom_line, "point" for geom_point
#' @param facet_by_time character, or FALSE. If FALSE, the whole time series
#'   will be displayed on a single graph (which can cause overplotting if it is
#'   will be displayed on a single graph (which can cause overplotting if it is
#'   long). Otherwise, please specify the name of the time column you wish to
#'   use to facet (=break into different plots), e.g. "year", or "month" if you
#'   have a single year. For multiple years to be displayed by month, you can
#'   create a composite column (e.g. \code{year_month = paste(output$year,
#'   output$month, sep = "_")}).
#' @param save_graph logical, whether the graph should be saved or not
#' @param path_save  if save_graph = TRUE the location of the file
#' @param filename if save_graph = TRUE the name of the file
#' @param device if save_graph = TRUE, the device used (see \code{?ggplot2::ggsave()} for more details)
#' @param ... additional graphical parameter for the corresponding geom.
#'
#' @return
#' @export
#'
#' @examples
display_SWM <- function(model_output,
                        what = "REW",
                        type = "line",
                        facet_by_time = "year",
                        save_graph = F,
                        path_save,
                        filename,
                        device,
                        ...){

  # General function ----------------------------------------------------------------------
  if(!require(ggplot2)) stop("Please install ggplot2 before using this function")

  if(length(what) > 1){
    if(all(what%in%c("REW","Drainage"))){
      plot <- .facet_SWM_outputs(model_output = model_output, what = what, type = type, facet_by_time=facet_by_time, ...)
    }
    else stop("argument what must be one or several of: 'REW', 'Drainage' (if relevant)")
  }
  else if(!what%in%c("REW","drainage","restTr")){
    stop("argument what must be one or several of: 'REW', 'Drainage'")
  }
  else if("Drainage" %in% what & !"Drainage" %in% names(model_output))
    stop("You asked for displaying Drainage values but it seems like you did not run_original_SWM with drainage=TRUE.")
  else plot <- .display_single_SWM_output(model_output = model_output, what = what, type = type,facet_by_time=facet_by_time, ...)

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
                               facet_by_time = FALSE,
                               ...){

  # Facets ------------------------------------------------------------------


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

  data <- model_output

  if(isFALSE(facet_by_time)){
    data <- data[,c("date",what)]
    data <- tidyr::gather(data=data, variable, value, -date)
  }
  else if(!facet_by_time %in% names(data)){
    stop("The column you indicated in facet_by_time argument is not in the dataset")
  }
  else{
    data <- data[,c("date",what, facet_by_time)]
    data <- tidyr::gather(data=data, variable, value, -date, -facet_by_time)
  }


  # stop("ok")
  print(head(data))
  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = date, y = value))+
    do.call(getExportedValue("ggplot2",paste0("geom_",type)),args = geom_args)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)

  if(!isFALSE(facet_by_time)){
    plot <- plot+
      ggplot2::facet_wrap(facets = c("variable",facet_by_time), scales = "free")
  }
  else{
    plot <- plot+
      ggplot2::facet_wrap(~variable, scales = "free_y")
  }
  return(plot)

}
.display_single_SWM_output <- function(model_output,
                                       what,
                                       type,
                                       facet_by_time = F,
                                       ...){

  # Single var --------------------------------------------------------------


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

  data <- model_output

  if(isFALSE(facet_by_time)){
    data <- data[,c("date",what)]
    data <- tidyr::gather(data=data, variable, value, -date)
  }
  else if(!facet_by_time %in% names(data)){
    stop("The column you indicated in facet_by_time argument is not in the dataset")
  }
  else{
    data <- data[,c("date",what, facet_by_time)]
    data <- tidyr::gather(data=data, variable, value, -date, -facet_by_time)
  }



  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = date, y = value))+
    do.call(getExportedValue("ggplot2",paste0("geom_",type)),args = geom_args)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)
  if(!isFALSE(facet_by_time)){
    plot <- plot+
      ggplot2::facet_wrap(facets = c("variable",facet_by_time), scales = "free")
  }

  return(plot)
}

# Function to display stacks ----------------------------------------------


#' Display multiple SWM outputs
#'
#' THis function displays multiple SWM function outputs stacked using stack_SWM function.
#'
#' @param model_stack data.frame and SWM_stack. The output of stack_SWM, corresponding to daily aggregated outputs for several simulations.
#' @inheritParams  display_SWM
#'
#' @return
#' @export
#'
#' @examples
display_SWM_stack <- function(model_stack,
                              what = "REW",
                              type = "line",
                              facet_by_time = F,
                              save_graph = F,
                              path_save,
                              filename,
                              device,
                              ...){

  # General -----------------------------------------------------------------

  if(!require(ggplot2)) stop("Please install ggplot2 before using this function")
  if(length(what) > 1){
    if(all(what%in%c("REW","Drainage"))){
      plot <- .facet_SWM_outputs(model_stack = model_stack, what = what, type = type, facet_by_time = facet_by_time,...)
    }
    else stop("argument what must be one or several of: 'REW', 'drainage'")
  }
  else if(!what%in%c("REW","Drainage")){
    stop("argument what must be one or several of: 'REW', 'drainage'")
  }
  else plot <- .display_single_stacked_variable(model_stack = model_stack, what = what, type = type, facet_by_time = facet_by_time, ...)

  if(save_graph){
    sv <- try(ggplot2::ggsave(file.path(path_save, filename), plot = plot, device = device))
    if(inherits(sv, "try-error")){
      message("The graph could not be saved. Please check the filename and path you specified.")
    }
  }
  return(plot)
}

.display_single_stacked_variable <- function(model_stack,
                                             what,
                                             type,
                                             facet_by_time = "year",
                                             ...){

  # Single var --------------------------------------------------------------


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
           # "colour" = {
           #   geom_args <- c(geom_args, coulour = .dots[[n]])
           # },
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

  data <- model_stack
  if(isFALSE(facet_by_time)){
    data <- data[,c("date","SWM_name",what)]
    data <- tidyr::gather(data=data, variable, value, -date, -SWM_name)
  }
  else if(!facet_by_time %in% names(data)){
    stop("The column you indicated in facet_by_time argument is not in the dataset")
  }
  else{
    data <- data[,c("date","SWM_name",what, facet_by_time)]
    data <- tidyr::gather(data=data, variable, value, -date, -SWM_name, -facet_by_time)
    # ?gather
  }


  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = date, y = value, colour = SWM_name))+
    do.call(getExportedValue("ggplot2",paste0("geom_",type)),args = geom_args)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)
  if(!isFALSE(facet_by_time)){
    plot <- plot + facet_wrap(facets = facet_by_time, scales = "free_x")
  }
  return(plot)
}

.facet_SWM_outputs <- function(model_stack,
                               what,
                               type,
                               facet_by_time = F,
                               ...){

  # Facets ------------------------------------------------------------------


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

  data <- model_stack
  if(isFALSE(facet_by_time)){
    data <- data[,c("date","SWM_name",what)]
  }
  else if(!facet_by_time %in% names(data)){
    stop("The column you indicated in facet_by_time argument is not in the dataset")
  }
  else{
    data <- data[,c("date","SWM_name",what, facet_by_time)]
  }
  # data <- data[,c("date","SWM_name",what)]
  data <- tidyr::gather(data=data, variable, value, -date, -SWM_name)
  # stop("ok")
  # print(head(data))
  plot <- ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = date, y = value, colour = SWM_name))+
    do.call(paste0("ggplot2::geom_",type),args = geom_args)+
    ggplot2::ggtitle(label = title, subtitle = subtitle)
  if(!isFALSE(facet_by_time)){
   plot <- plot +
      ggplot2::facet_wrap(facets = c("variable",facet_by_time), scales = "free")
  }
  else{
    plot <- plot +
      ggplot2::facet_wrap(~variable, scales = "free_y")
  }
  return(plot)

}
