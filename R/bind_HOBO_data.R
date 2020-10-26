#' @title bind_HOBO_data: Binds sentinel well data using data files that are exported from
#' HoboWare as .csv files (for internal use).
#'
#' @importFrom dplyr mutate select
#' @importFrom lubridate date
#'
#' @description This function finds the well data files based on a specified path
#' and partial matching of files based on site name, reads these files into R,
#' adds Well_ID and site name to each file, then binds them together and prepares
#' the data to be imported into the MS Access RAM backend table: tbl_Water_Level.
#' If appending to the Access table is not needed, then drop the first column 'ID',
#' which is left blank so that Access can assign an auto number for the primary key.
#' This function is mostly for internal use to generate a long format of the data
#' to append to the MS Access table. Function requires all 8 sites and 2 barometric
#' loggers to run. Mostly for internal use.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#' @param export \code{TRUE} or \code{FALSE}. Export csv file to specified path. Defaults to \code{TRUE}.
#'
#' @examples
#' dir = c('C:/Water_level_data/growing_season_2019')
#' well_data <- bind_HOBO_data(path)
#'
#' @return Returns a long data frame with ID (blank for Access to assign autonumber), Well_ID,
#' timestamp, Absolute_Pressure_kPa, Degrees_C, and site.
#'
#' @export

bind_HOBO_data <- function(path, export = TRUE){
  path <- if(substr(path, nchar(path), nchar(path))!="/"){
    paste0(path, "/")
    } else(paste0(path))

  filenames <- list.files(path = path, pattern =".csv")

  duck <- tryCatch(df <- read.table(paste0(path, filenames[grep('Duck', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('Duck', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  lihu <- tryCatch(df <- read.table(paste0(path, filenames[grep('LittleHunter', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('LittleHunter', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  gilm <- tryCatch(df <- read.table(paste0(path, filenames[grep('Gilmore', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('Gilmore', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )


  wmtn <- tryCatch(df <- read.table(paste0(path, filenames[grep('WMTN_well', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('WMTN_well', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  hodg <- tryCatch(df <- read.table(paste0(path, filenames[grep('Hodgdon', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('Hodgdon', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  nemi <- tryCatch(df <- read.table(paste0(path, filenames[grep('NewMills', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('NewMills', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  hebr <- tryCatch(df <- read.table(paste0(path, filenames[grep('Heath_Brook', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('Heath_Brook', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  bigh <- tryCatch(df <- read.table(paste0(path, filenames[grep('Big_Heath', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('Big_Heath', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  wmtn_baro <- tryCatch(df <- read.table(paste0(path, filenames[grep('westmtnswmp', filenames, ignore.case = TRUE)]),
                                    skip = 1, sep = ',', stringsAsFactors = FALSE,
                                    col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                   error = function(e){
                     df <- read.table(paste0(path, filenames[grep('westmtnswmp', filenames, ignore.case = TRUE)]),
                                      skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                     colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                     return(df)
                   }

  )

  shed_baro <- tryCatch(df <- read.table(paste0(path, filenames[grep('SHED', filenames, ignore.case = TRUE)]),
                                         skip = 1, sep = ',', stringsAsFactors = FALSE,
                                         col.names = c('V1', 'Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C'))[-1,2:4],
                        error = function(e){
                          df <- read.table(paste0(path, filenames[grep('SHED', filenames, ignore.case = TRUE)]),
                                           skip = 1, sep = ',', stringsAsFactors = FALSE)[-1,2:4]
                          colnames(df) <- c('Measure_Date_Time', 'Absolute_Pressure_kPa', 'Degrees_C')
                          return(df)
                        }

  )

  duck <- duck %>% mutate(site = 'duck', Well_ID = 9, ID = NA)
  lihu <- lihu %>% mutate(site = 'lihu', Well_ID = 13, ID = NA)
  gilm <- gilm %>% mutate(site = 'gilm', Well_ID = 12, ID = NA)
  wmtn <- wmtn %>% mutate(site = 'wmtn', Well_ID = 10, ID = NA)
  hodg <- hodg %>% mutate(site = 'hodg', Well_ID = 16, ID = NA)
  nemi <- nemi %>% mutate(site = 'nemi', Well_ID = 14, ID = NA)
  hebr <- hebr %>% mutate(site = 'hebr', Well_ID = 15, ID = NA)
  bigh <- bigh %>% mutate(site = 'bigh', Well_ID = 11, ID = NA)
  wmtn_baro <- wmtn_baro %>% mutate(site = 'wmtn_baro', Well_ID = 17, ID = NA)
  shed_baro <- shed_baro %>% mutate(site = 'shed_baro', Well_ID = 19, ID = NA)

  combdata <- rbind(duck, lihu, gilm, wmtn, hodg, nemi, hebr, bigh, wmtn_baro, shed_baro)

  combdata <- combdata %>% mutate(Measure_Date_Time = as.POSIXct(Measure_Date_Time,
                                                                   format = "%m/%d/%y %I:%M:%S %p",
                                                                   tz = "America/New_York"),
                                timestamp = as.POSIXct(Measure_Date_Time,
                                                       format= "%Y-%m-%d %H:%M:%S",
                                                       tz = "America/New_York"),
                                Flag = paste0("A"),
                                Flag_Note = NA)

  combdata <- combdata %>% select(ID, Well_ID, Measure_Date_Time, Absolute_Pressure_kPa,
                                  Degrees_C, Flag, Flag_Note)

  if(export == TRUE){

    filename <- paste0("HOBO_data_for_database_",
                       lubridate::date(Sys.time()), ".csv")

    write.csv(combdata, paste0(path, filename), row.names = FALSE)

    cat(paste0("File: ", filename, " saved to: ", "\n", "\t",
                                    path))
  }

  return(combdata)
  }