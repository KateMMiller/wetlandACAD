#' @title compile_WL_data: Converts well data to water level
#'
#' @importFrom dplyr filter mutate rename right_join select
#' @importFrom lubridate year yday force_tz
#' @importFrom tidyr pivot_wider
#'
#' @description This function pulls in the water level data from the NETN RAM database,
#' joins the location, well visit, and raw logger pressure data to calculate water level
#' relative to wetland surface. Function only includes data between the spring and fall
#' well visit per year. At least 1 site of water level data is required.
#' \strong{Must have a the NETN RAM backend database named as a DSN.}
#' Function has been updated for new loggers that return differential pressure, which
#' is the difference between absolute pressure and barometric pressure. Function is
#' primarily for internal use, and only works with data collected from 10/28/2024 and later.
#' Data collected prior to 10/28/2024 should be analyzed using version 0.1.5 of this package.
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
#' compile_WL_data(path = dir, year = 2025, export = TRUE, growing_season = TRUE)
#'
#' # Assign output from all 2018 data, including flagged records, to global environment
#' wl_2025 <- compile_WL_data(year = 2025, rejected = TRUE, growing_season = FALSE,
#'                                    export = FALSE)
#'
#' # Run for 2025 growing season data without printing messages in the console, and save output to file
#' wl_2025 <- compile_WL_data(year = 2025, growing_season = TRUE, export = TRUE,
#'                                    quietly = TRUE)
#' }
#'
#' @return Returns a wide data frame with timestamp and SITECODE_WL.
#'
#' @export
#'

compile_WL_data <- function(path = NULL, year = as.numeric(format(Sys.Date(), "%Y")),
                            rejected = FALSE, growing_season = TRUE,
                            export = TRUE, quietly = FALSE){


# Import database tables
db <- DBI::dbConnect(drv = odbc::odbc(), dsn="RAM_BE")
if(quietly == FALSE) {cat("Importing data tables from NETN RAM database")}
assign("well_visit", DBI::dbReadTable(db, "tbl_Well_Visit"), envir = .GlobalEnv)
assign("well_loc", DBI::dbReadTable(db, "tbl_Well"), envir = .GlobalEnv)
if(!exists("raw_wl")){ # object assigned in well_02_prep_well_data(), but added here in case.
  assign("raw_wl", DBI::dbReadTable(db, "tbl_Water_Level"), envir = .GlobalEnv)
}
DBI::dbDisconnect(db)

# Preparing raw water level data
raw_wl_yr1 <- raw_wl |>
  mutate(logger_time = force_tz(Measure_Date_Time, tzone = "America/New_York"),
         Measure_Date_Time = force_tz(Measure_Date_Time, tzone = "America/New_York")) |>
  filter(year(logger_time) == year) |>
  mutate(WL_raw_cm = Differential_Pressure_kPa * 10.197,
         Year = lubridate::year(Measure_Date_Time),
         month = lubridate::month(Measure_Date_Time),
         hour = lubridate::hour(Measure_Date_Time),
         hour_pad = sprintf("%02d", hour),
         doy = lubridate::yday(Measure_Date_Time),
         doy_h = as.numeric(paste0(doy, ".", hour_pad))) |>
  select(-ID)

raw_wl_yr <- left_join(raw_wl_yr1, well_loc[,c("ID", "Site_Code")], by = c("Well_ID" = "ID"))

# Preparing well visit data to relate stick up and other measurements to loggers
well_visit2 <- right_join(well_loc |> select(ID, Site_Code, Logger_Length, MP_to_Bolt),
                          well_visit,
                          by = c("ID" = "Well_ID")) |>
  mutate(Year = lubridate::year(Visit_Date),
         month = lubridate::month(Visit_Date),
         season = ifelse(between(month, 4, 6), "spring",
                         ifelse(between(month, 9, 11), "fall", "offseason")),
         hour = lubridate::hour(Water_Depth_Time),
         water_depth_time = paste0(ymd(Visit_Date), " ",
                                   sprintf("%02d", hour),
                                   ":00:00"),
         ground = (Logger_Length + MP_to_Bolt) - Stick_Up_at_MP) |>
  filter(Year %in% year)

# Check for offseason visits
if(dim(well_visit2 |> filter(season %in% "offseason") %>% droplevels())[1]>0){
  message(paste("Warning: There are well visits outside of spring or fall periods. ",
                "Offseason measurements are omitted from this function",
                sep = "\n"))
}

# Fix time format
well_visit2$water_depth_time <- as.POSIXct(well_visit2$water_depth_time,
                                           format = "%Y-%m-%d %H:%M:%S",
                                           tz = "America/New_York" )
well_visit3 <- well_visit2 |>
  filter(season %in% 'spring') |> # only use spring stickup measurement for logger calcs
  select(Site_Code, Visit_Date, Year, month,
         water_depth_time, season, Water_Depth, ground, Stick_Up_at_MP)

# Join logger to well visit data for length measurements
wl_well <- left_join(raw_wl_yr,
                     well_visit3 |> select(Site_Code, ground, Water_Depth, Stick_Up_at_MP),
                     by = "Site_Code") |>
  mutate(WL_cm = WL_raw_cm - ground)

# library(ggplot2)
# ggplot(wl_well, aes(x = Measure_Date_Time, y = WL_raw_cm, group = Site_Code, color = Site_Code)) +
#   geom_line()# + facet_wrap(~Site_Code)
#
# ggplot(wl_well |> filter(Site_Code == "GILM"),
#        aes(x = Measure_Date_Time, y = WL_cm, group = Site_Code, color = Site_Code)) +
#   geom_line() + facet_wrap(~Site_Code)

# Check field vs logger WL

# first and last logger measurement
last_wl <-
    wl_well |> group_by(Site_Code) |>
      slice_max(Measure_Date_Time) |>
      mutate(season = "fall") |>
      data.frame() |>
  select(Site_Code, water_depth_time = Measure_Date_Time, season, WL_cm)

first_wl <- left_join(well_visit2 |> filter(season == "spring") |>
                        select(Site_Code, water_depth_time, season),
                      wl_well |> select(Site_Code, Measure_Date_Time, WL_cm),
                      by = c("Site_Code", "water_depth_time" = "Measure_Date_Time"))

first_last_wl <- rbind(first_wl, last_wl)

well_visit_wl <- well_visit2 |>
  mutate(Field_WL = Stick_Up_at_MP - Water_Depth,
         ground = (Logger_Length + MP_to_Bolt) - Stick_Up_at_MP) |>
  select(ID, Site_Code, Visit_Date, Year, water_depth_time, season, ground, Stick_Up_at_MP, Field_WL)

wl_field_check <- left_join(well_visit_wl, first_last_wl,
                      by = c("Site_Code", "season"),
                      suffix = c("_visit", "_logger")) |>
  mutate(wl_diff = WL_cm - Field_WL)

assign("wl_field_check", wl_field_check, envir = .GlobalEnv)
# measurements aren't great for WMTN, but not sure why.
# assigning to global env. to look over

# Reshape to wide
wl_wide <- wl_well |> select(Site_Code, Measure_Date_Time, Year, month, hour, doy, doy_h, WL_cm) |>
  pivot_wider(names_from = Site_Code, values_from = WL_cm, names_glue = "{Site_Code}_WL")

return(wl_wide)

}

