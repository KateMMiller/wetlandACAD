#' @title prep_well_data: Prepares sentinel well data for conversion to water level using data tables stored in the NETN RAM database
#' (for internal use).
#'
#' @importFrom dplyr between case_when rename
#' @importFrom magrittr %>%
#' @importFrom lubridate year yday
#' @importFrom odbc odbc odbcListDataSources
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom tidyr pivot_wider
#'
#' @description This function pulls in the water level data from the NETN RAM database,
#' joins the location, well visit and water level data together into a wide
#' format of the data with the timestamp as the joining column. Function requires
#' all 8 sites and 2 barometric loggers to run, and only includes data between the
#' spring and fall well visit per year. Must have a the NETN RAM backend database named
#' as a DSN.Function is primarily for internal use.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#' @param from First year to start joining the data
#' @param to Last year to end the join
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
#' # Export growing season only data to a table
#' dir='D:/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Fall_2019'
#' prep_well_data(path = dir, from = 2019, to = 2019, export = TRUE, growing_season = TRUE)
#'
#' # Assign output from all 2018 data to global environment
#' welld_2018 <- prep_well_data(from = 2018, to = 2018, export = FALSE, growing_season = FALSE)
#'
#' # Run without printing messages in the console
#' welld_2018 <- prep_well_data(from = 2018, to = 2018, export = FALSE, quietly = TRUE)
#'
#' @return Returns a wide data frame with timestamp, SITENAME_AbsPres, SITENAME_C.
#'
#' @export

prep_well_data<-function(path = NULL, from = 2013, to = 2019, rejected = FALSE, growing_season = TRUE,
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
  if(missing(path)) {path = "C:/Temp"}

  if(export == TRUE & dir.exists(path) == FALSE){
    stop(paste0("The specified path: ", path, " does not exist"))
  }

  # Error handling for specified path
  path <- if (substr(path, nchar(path), nchar(path)) != "/") {
    paste0(path, "/")
  } else {(paste0(path))}

  # Check that RAM_BE is a named user or system DSN
  if(dim(odbc::odbcListDataSources() %>% filter(name == "RAM_BE"))[1] == 0)
    stop ('Compile function failed. There is no DSN named "RAM_BE".')

  #----------------
  # Import data from Access database
  #----------------
  if(quietly == FALSE) {cat("Importing tbl_Water_Level from NETN RAM database.")}
  db <- DBI::dbConnect(drv=odbc::odbc(), dsn="RAM_BE")
  assign("raw_wl", DBI::dbReadTable(db, "tbl_Water_Level"), envir=.GlobalEnv)
  if(quietly == FALSE) {cat("....")}
  DBI::dbDisconnect(db)
  if(quietly == FALSE) {cat("Done.", sep="\n")}

  #----------------
  # Combining tables
  #----------------
  # Add site code and join location to water level data
  if(quietly == FALSE) {cat("Preparing and reshaping data from long to wide.")}

  raw_wl2 <- raw_wl %>% mutate(code = as.factor(case_when(Well_ID == 9  ~ "DUCK",
                                                Well_ID == 10 ~ "WMTN",
                                                Well_ID == 11 ~ "BIGH",
                                                Well_ID == 12 ~ "GILM",
                                                Well_ID == 13 ~ "LIHU",
                                                Well_ID == 14 ~ "NEMI",
                                                Well_ID == 15 ~ "HEBR",
                                                Well_ID == 16 ~ "HODG",
                                                Well_ID == 17 ~ "WMTN.BARO",
                                                Well_ID == 18 ~ "MARS.BARO",
                                                Well_ID == 19 ~ "SHED.BARO")),
                               year = year(Measure_Date_Time),
                               doy = yday(Measure_Date_Time)) %>%
    select(-ID, -Well_ID) %>% rename(AbsPres = Absolute_Pressure_kPa, C = Degrees_C)

  # Check for duplicate timestamps, which will cause an error in pivot_wider
  dups<-raw_wl2[(which(duplicated(raw_wl2[,c("Measure_Date_Time", "code")]))),]

  if(dim(dups)[1]>0){
    assign("dup_records", dups, envir=.GlobalEnv)
    }

  if(dim(dups)[1]>0){
    stop(paste0("Error: There are ",dim(dups)[1], " duplicate records in MS Access tbl_Water_Level. ",
                 "The data frame of duplicate records is named dup_records in the global environment"))
  }

  # Filter data based on function arguments
  raw_wl3 <- raw_wl2 %>% filter(between(year, from, to))

  raw_wl4 <- if(rejected == FALSE){
    raw_wl3 %>% filter(Flag != "R") %>% select(-Flag, -Flag_Note)
  } else if(rejected == TRUE) {raw_wl3 %>% select(-Flag, -Flag_Note)}

  raw_wl5 <- if(growing_season == TRUE){
    raw_wl4 %>% filter(doy > 134 & doy < 275)
  } else {raw_wl4}

  if(quietly == FALSE) {cat("..")}

  wl_wide <- raw_wl5 %>% pivot_wider(names_from = c(code), values_from = c(AbsPres, C)) # names are AbsPres_SITE

  if(quietly == FALSE) {cat("..")}

  wl_wide2 <- wl_wide %>% setNames((nm = sub("(.*)_(.*)", "\\2_\\1", names(.)))) %>% # Change name order to SITE_AbsPres.
    setNames(nm=sub("\\.", "_", names(.))) %>% # extra step for BARO loggers
    select(-year) %>%
    rename(timestamp = Time_Measure_Date)

  if(quietly == FALSE) {cat("Done.", sep="\n")}

  if(export == TRUE){
    filename1 <- if(from != to) {
      paste0("raw_well_data_", from, "-", to)
    } else {paste0("raw_well_data_", to)}

    filename <- if(growing_season == TRUE){
      paste0(filename1, "_GS.csv")} else {paste0(filename1, ".csv")}

    write.csv(wl_wide2, paste0(path, filename), row.names = FALSE)

    if(quietly == FALSE){cat(paste0("File: ", filename, " saved to: ", "\n", "\t",
               path))}
  }

  return(wl_wide2)

}
