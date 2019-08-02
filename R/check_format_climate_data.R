#' Check and format climate data to run the SWM.
#'
#' This function takes your climate data as an input, parses the date with
#' lubridate according to the format you specify, checks and formats the climate
#' date to input in the SWM. It handles timesteps other than daily, and values
#' of PET or light intensity different to default values of the model.
#'
#' @param climate_data data.frame, time series containing a date with homogen
#'   format, and at least the corresponding rainfall data. It can also contain
#'   PET and light intensity data, in which case the corresponding column names
#'   have to be specified in PET_col and light_intensity_col, respectively.
#' @param rainfall_col single character, the name of the column containing
#'   rainfall measurements.
#' @param timestep single character, defaults to "daily". If you input rainfall
#'   data with monthly timestep (average), set this argument to "monthly".
#'   Timesteps other than daily yields model outputs that have poorer temporal
#'   resolution, and are not very comparable to the original model outputs
#'   presented in Wagner et. al (2010). Using an "annual" timestep is probably
#'   pointless, yet I included this possibility in the code.
#' @param date_col  single character, the name of the column containing
#'   measurement dates. Dates have to be homogenously formatted.
#' @param date_format single character, one of the following:
#'   "ymd","ydm","dmy","dym","mdy","myd". y:year, m:month, d:day.
#' @param PET_col Optional. single character that defaults to NULL, the name of
#'   the column containing potential evapotranspiration values to be used in the
#'   model. If not specified, a constant, averaged daily light intensity equal
#'   to 586.8 (from Guyaflux data) will be used as in the original model
#'   presented by Wagner et al. (2010). This is recommended if you want to
#'   obtain comparable result, yet the code structure technically allows to use
#'   real measures of daily light intensity.
#' @param light_intensity_col Optional. single character that defaults to NULL,
#'   the name of the column containing daily light intensity values to be used
#'   in the model. If not specified, a constant, averaged daily light intensity
#'   equal to 3.97 (from Guyaflux data) will be used as in the original model
#'   presented by Wagner et al. (2010). This is recommended if you want to
#'   obtain comparable result, yet the code structure technically allows to use
#'   real measures of daily light intensity.

#'
#' @details
#'
#' The function in lubridate package corresponding to the specified date format
#' is internally called to parse characters into Date objects. If parsing
#' totally of partially fails (returning NA for the corresponding lines), this
#' can be due to either non-homogenous date format across your dataset, or to a
#' wrong specification of the format. This function is not designed to tidy-up
#' the formats of your dataset's dates, and if any parsing fails, it will stop
#' with an error message. In this case, you have to check your date formats
#' before re-running the function.
#'
#' @return
#'
#' A data.frame with 4 columns: date, rainfall, PET and light_intensity, with
#' daily timestep. If input rainfalls are monthly averages or cumulative monthly
#' averages, the output rainfalls will be average daily rates that are
#' calculated accordingly.
#'
#' @export
#'
#' @examples
check_format_climate_data <- function(climate_data,
                                      rainfall_col = "pre",
                                      timestep = "daily",
                                      date_col = "date",
                                      date_format = "dmy",
                                      PET_col = NULL,
                                      light_intensity_col = NULL){


# Checks ------------------------------------------------------------------


  if(!is.data.frame(climate_data)) stop("climate_data must be a data.frame")
  else if(length(class(climate_data)) > 1){
    climate_data <- as.data.frame(climate_data)
    }

  if(!all(is.character(c(rainfall_col, timestep, date_col, date_format))) |
     !all(c(length(rainfall_col),length(timestep),length(date_col),length(date_format)) == 1)) stop("rainfall_col, timestep, date_col and date_format must all be characters of length 1.")

  if(!date_col %in% names(climate_data)) stop("rainfall_col must be the name of the column containing dates.")
  if(!rainfall_col %in% names(climate_data)) stop("rainfall_col must be the name of the column containing rainfall measurements.")

  if(!date_format %in% c("ymd","ydm","dmy","dym","mdy","myd")) stop('date_format must be one of the following: "ymd","ydm","dmy","dym","mdy","myd" ')



# Check rainfall values ---------------------------------------------------

  # print(length(class(climate_data)))
  rainfall <- climate_data[,which(names(climate_data) == rainfall_col)]
  # print(rainfall)
  # print(class(rainfall))
  if(!is.numeric(rainfall)) stop("rainfall values must be numeric...")
# Check PET and light intensity if specified or set it to default ---------


  if(!is.null(PET_col)){
    if(!is.character(PET_col)| length(PET_col != 1)){
      stop("PET_col must be a character of length 1 indicating the name of climate_data's column containing PET values")
    }
    else PET <- climate_data[,which(names(climate_data == PET_col))]
  }
  else PET <- rep(586.8, length(rainfall))

  if(!is.null(light_intensity_col)){
    if(!is.character(light_intensity_col)| length(light_intensity_col != 1)){
      stop("light_intensity_col must be a character of length 1 indicating the name of climate_data's column containing light intensity values")
    }
    else light_intensity <- climate_data[,which(names(climate_data == light_intensity_col))]
  }
  else light_intensity <- rep(3.97, length(rainfall))


# Check date class, parse it if possible and necessary --------------------
  date <- climate_data[,which(names(climate_data)==date_col)]
  if(!is.character(date)){
    if(is.factor(date)){
      date <- as.character(date)
    }
    else if(!lubridate::is.instant(date)){
      stop("the dates you inputted are neither a character (or a factor coercicible to character) nor an instant (see lubridate's vignette). We cannot deal with other types than these")
    }
  }

  date <- do.call(getExportedValue("lubridate", date_format),list(date))



# Generate the output table according to the timestep ! -------------------


 if(timestep != "daily"){
   if(!timestep %in% c("monthly","annual")) stop("Unrecognised timestep. Please read the documentation.")

   if(timestep == "monthly"){
     data <- do.call("rbind", lapply(1:length(date), function(i)
       data.frame(date = seq(date[i],
                             (seq(date[i],length=2,by="months") - 1)[2], by = "1 days"),
                  rainfall = rainfall[i],
                  PET = PET[i],
                  light_intensity =light_intensity[i])))
   }
   else if(timestep == "annual"){
     message("Using annual timestep data is highly non-recommended. Do whatever you want, but please keep this in mind.")
     data <- do.call("rbind", lapply(1:length(date), function(i)
       data.frame(date = seq(date[i],
                             (seq(date[i],length=2,by="year") - 1)[2], by = "1 days"),
                  rainfall = rainfall[i],
                  PET = PET[i],
                  light_intensity =light_intensity[i])))
   }
 }
  else(data <- data.frame(date = date, rainfall = rainfall, PET = PET, light_intensity = light_intensity))

  return(data)
}
