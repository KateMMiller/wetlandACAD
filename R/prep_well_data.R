#' @title prep_well_data: Converts well data to water level
#'
#' @importFrom dplyr filter mutate rename right_join select
#' @importFrom lubridate year yday force_tz
#' @importFrom tidyr pivot_wider
#'
#' @description This function pulls in the water level data from the NETN RAM database,
#' joins the location, well visit and water level data together into a wide
#' format of the data with the timestamp as the joining column. Function only includes
#' data between the spring and fall well visit per year. At least 1 site of water level
#' data is required. \strong{Must have a the NETN RAM backend database named as a DSN.} Function has been
#' updated for new loggers that log water level in cm and do not require pressure corrections.
#' Function is primarily for internal use.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#' @param year Numeric. The year you are preparing the data for. Function will only run 1 year at a time.
#' @param rejected \code{TRUE} or \code{FALSE}. If \code{TRUE} will include flagged rejected records.
#' If \code{FALSE} will replace the rejected flagged records with NA. Defaults to \code{FALSE}.
#' @param growing_season \code{TRUE} or \code{FALSE}. If \code{TRUE} will include only growing season
#' measurements, defined as May 15 to Oct. 1. If \code{FALSE}, will return all measurements.
#' If growing_season = \code{TRUE} and export = \code{TRUE}, the output filename will include "GS"
#' Defaults to \code{TRUE}.
#' @param export \code{TRUE} or \code{FALSE}. Export csv file to specified path. Defaults to \code{TRUE}.
#' @param quietly \code{TRUE} or \code{FALSE}. If \code{FALSE}, code will not print progress into console.
#' Defaults to \code{FALSE}
#'
#' @examples
#' \dontrun{
#' # Export growing season only data to a table
#' dir = c('C:/Water_level_data/growing_season_2025')
#' prep_well_data(path = dir, year = 2025, export = TRUE, growing_season = TRUE)
#'
#' # Assign output from all 2018 data, including flagged records, to global environment
#' welld_2018 <- prep_well_data(year = 2025, rejected = TRUE, growing_season = FALSE,
#'                              export = FALSE)
#'
#' # Run for 2019 growing season data without printing messages in the console, and save output to file
#' welld_2018 <- prep_well_data(year = 2025, growing_season = TRUE, export = TRUE,
#'                              quietly = TRUE)
#' }
#'
#' @return Returns a wide data frame with timestamp, SITENAME_AbsPres.
#'
#' @export
#'

prep_well_data <- function(path = NULL, year = as.numeric(format(Sys.Date(), "%Y")),
                           rejected = FALSE, growing_season = TRUE,
                           export = TRUE, quietly = FALSE){

  #----------------
  # Error handling
  #----------------
  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Check that specified path exists on computer
  if(export == TRUE & dir.exists(path) == FALSE){
    stop(paste0("The specified path: ", path, " does not exist"))
  }

  # Error handling for specified path
  path <- if (substr(path, nchar(path), nchar(path)) != "/") {
    paste0(path, "/")
  } else {(paste0(path))}

  # Check that RAM_BE is a named user or system DSN
  if(dim(odbc::odbcListDataSources() %>% filter(name == "RAM_BE"))[1] == 0){
    stop('Compile function failed. There is no DSN named "RAM_BE".')}

  if(length(year) != 1){
    stop("Too many years specified. Function will only run for 1 year at a time.")
  }

  #----------------
  # Import data from Access database
  #----------------
  if(quietly == FALSE) {cat("Importing tbl_Water_Level from NETN RAM database.")}
  db <- DBI::dbConnect(drv = odbc::odbc(), dsn = "RAM_BE")
  assign("raw_wl", DBI::dbReadTable(db, "tbl_Water_Level"), envir = .GlobalEnv)
  if(quietly == FALSE) {cat("..")}
  assign("well_loc", DBI::dbReadTable(db, "tbl_Well"), envir = .GlobalEnv)
  if(quietly == FALSE) {cat("..")}
  DBI::dbDisconnect(db)
  if(quietly == FALSE) {cat("Done.", sep = "\n")}

  #----------------
  # Combining tables
  #----------------
  # Add site code and join location to water level data
  if(quietly == FALSE) {cat("Preparing and reshaping data from long to wide.")}

  raw_wl2 <- right_join(well_loc[ , c("ID", "Site_Code")], raw_wl,
                        by = c("ID" = "Well_ID"))

  raw_wl3 <- raw_wl2 |> mutate(Year = year(Measure_Date_Time),
                               doy  = yday(Measure_Date_Time))  |>
    select(-ID, -ID.y, -Degrees_C)  |>
    rename(AbsPres = Absolute_Pressure_kPa)  |>
    filter(Year == year)

  # Check for duplicate timestamps, which will cause an error in spread
  dups <- raw_wl3[(which(duplicated(raw_wl3[,c("Measure_Date_Time", "Site_Code")]))),]

  if(dim(dups)[1]>0){
    assign("dup_records", dups, envir = .GlobalEnv)
    }

  if(dim(dups)[1]>0){
    stop(paste0("Error: There are ", dim(dups)[1], " duplicate records in MS Access tbl_Water_Level. ",
                 "The data frame of duplicate records is named dup_records in the global environment"))
  }

  raw_wl4 <- if(rejected == FALSE){
    raw_wl3 |> filter(Flag != "R") |> select(-Flag, -Flag_Note)
  } else if(rejected == TRUE) {raw_wl3 |> select(-Flag, -Flag_Note)}

  raw_wl5 <- if(growing_season == TRUE){
    raw_wl4 |> filter(doy > 134 & doy < 275)
  } else {raw_wl4}

  if(quietly == FALSE) {cat("..")}

  #wl_wide <- raw_wl6 %>% pivot_wider(names_from = c(Site_Code), values_from = c(AbsPres)) # names are AbsPres_SITE
  wl_wide <- raw_wl5 |> select(Measure_Date_Time, Water_Level_cm, Year, doy, Site_Code) |>
    pivot_wider(names_from = Site_Code, values_from = Water_Level_cm)

  if(quietly == FALSE) {cat("..")}

  cols <- paste0(names(wl_wide[,4:ncol(wl_wide)]), "_WL_raw")
  colnames(wl_wide) <- c("timestamp", "Year", "doy", cols)
  wl_wide$timestamp <- force_tz(wl_wide$timestamp, "America/New_York")

  if(quietly == FALSE) {cat("Done.", sep = "\n")}

  if(export == TRUE){
    filename <- if(growing_season == TRUE){
      paste0("raw_well_data_", year, "_GS.csv")
      } else {paste0("raw_well_data_", year, ".csv")}

    write.csv(wl_wide, paste0(path, filename), row.names = FALSE)

    if(quietly == FALSE){cat(paste0("File: ", filename, " saved to: ", "\n", "\t",
               path))}
  }

  return(wl_wide)

}
