#' @title bind_HOBO_data: Binds raw HOBO well data from csv
#'
#' @importFrom purrr list_rbind map
#'
#' @description This function imports individual raw HOBO well data files that were exported
#' as csvs in the HOBO app. Using the specified path in the function and assuming file structure
#' is such that the spring (October to April data) visit files are in a separate folder from fall
#' visit files, and that each site's file name contains with its 4-letter code (e.g. Duck Pond
#' Peatland's code is DUCK) as specified in the site_codes argument. The individual site data are
#' row bound to match the raw water level table that originally was tbl_Water_Level in the MS Access
#' RAM backend, but is now a stand-alone csv.
#'
#' This function generates a long format of the raw water level data to append to the existing water
#' level table. By default, function imports all 8 sentinel sites by their code. Changing site_codes
#' and well_id allows you to import differing number and names of sites. Note that each imported file
#' must contain the following columns: # (Index column), Date-Time (EDT/EST),
#' Differential Pressure (kPa), Absolute Pressure (kPa), Temperature (C), Water Level (m),
#' Barometric Pressure (kPa). If this hard coding causes difficulty, will revise function to be more
#' generic in future.
#'
#' Note that this function has been updated to work with data collected by MX2001 water level loggers
#' with built-in atmospheric pressure correction. Use package version 0.1.5 to compile water
#' level for HOBO U20 Loggers with external atmospheric pressure correction.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#' @param export /code{TRUE} or /code{FALSE}. Export csv file to specified path. Defaults to /code{TRUE}.
#' @param site_codes Quoted string of 4-letter site codes which are assumed to be in the file names
#' of the water level csvs to be imported. If importing all 8 sentinel sites without changes, default
#' settings do not need to be changed. If sites differ from default settings, in length or name, then both
#' site_codes and well_id must be specified and match in length.
#' @param well_id Numbered list of Well_ID matching tbl_Well in MS Access RAM database. If importing
#' all 8 sentinel sites without changes, default settings do not need to be changed. This is the primary
#' key used to link water level data to well visit data tables. The well_id should match the Well_ID record
#' from the tbl_Wells for a given site in the MS Access RAM backend.
#'
#' @examples
#' /dontrun{
#' dir = 'C:/Users/KMMiller/OneDrive - DOI/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Fall_2025/'
#'
#' well_data <- bind_HOBO_data(path)
#' }
#'
#' @return Returns a long data frame of raw water level data.
#'
#' @export

bind_HOBO_data <- function(path = NA,
                           export = TRUE,
                           site_codes = c("BIGH", "DUCK", "GILM", "HEBR",
                                          "HODG", "LIHU", "NEMI", "WMTN"),
                           well_id = c(11, 9, 12, 15, 16, 13, 14, 10)){

  if(is.na(path)){stop("Must specify a path.")}
  if(!dir.exists(path)){stop("Specified path directory does not exist.")}

  path <- if(substr(path, nchar(path), nchar(path))!="/"){
    paste0(path, "/")
    } else(paste0(path))

  site_well <- data.frame(site = site_codes, Well_ID = well_id)
  filenames_all <- list.files(path = path, pattern =".csv")

  # Add a check for missing files and return site name missing.
  filenames <- filenames_all[grepl(paste(site_codes, collapse = "|"), filenames_all)]

  if(length(filenames) < length(site_codes)){
    stop("Not all specified site_codes have an associated file.")
  }
  if(length(filenames) > length(site_codes)){
    stop("There are more files than specified site_codes.
         Can only import one file per site per specified path.")
  }

  orig_names = c("X", "DateTime", "DifferentialPressurekPa",
                 "AbsolutePressurekPa", "TemperatureC", "WaterLevelm",
                 "BarometricPressurekPa")

  num_cols = 1:length(orig_names)

  col_names = c("V1", "Measure_Date_Time", "Differential_Pressure_kPa", "Absolute_Pressure_kPa",
                "Degrees_C", "Water_Level_m", "Baro_Pressure_kPa")

  # Import function to iterate
  import_df <- function(site){
    tryCatch(
    df1 <- read.csv(paste0(path, filenames[grep(site, filenames, ignore.case = TRUE)]))[num_cols],
    error = function(e){stop(paste0("Did not find "))})
    df <- df1[!is.na(df1[,3]),]

    # Error handler for missing columns in dataframe.
    colnames(df) <- gsub("\\.", "", names(df))
    colnames(df) <- gsub("EDTEST|ESTEDT", "", names(df))
    missing_cols <- setdiff(orig_names, names(df))
    if(length(missing_cols) > 0){stop(
      paste0("The following columns are missing or mispelled from the imported dataset: ",
        paste0(missing_cols, collapse = ", ")))}

    df <- df[,orig_names] # setting order of df columns
    colnames(df) <- col_names
    df$site <- site
    df$Well_ID <- site_well$Well_ID[site_well$site == site]
    return(df)
  }

   # Import datasets
   fulld <-
     purrr::map(site_codes, function(site){import_df(site)}) |> purrr::list_rbind()

   # Set up timestamp
   fulld$Measure_Date_Time = as.POSIXct(fulld$Measure_Date_Time,
                                         format = "%m/%d/%Y %H:%M:%S",
                                         tz = "America/New_York")
   fulld$timestamp = as.POSIXct(fulld$Measure_Date_Time,
                                format = "%m/%d/%Y %H:%M:%S",
                                tz = "America/New_York")
   fulld$Flag = "A"
   fulld$Flag_Note = NA

  if(export == TRUE){
    filename <- paste0("Raw_HOBO_data_",
                       format(Sys.Date(), "%Y%m%d"), ".csv")

    write.csv(fulld, paste0(path, filename), row.names = FALSE)

    cat(paste0("File: ", filename, " saved to: ", path))
  }

  return(data.frame(fulld))
  }
