#' @include prep_well_data.R
#'
#' @title prep_conv_table: Prepares conversion table to calculate water level
#' relative to the wetland surface
#'
#' @importFrom dplyr arrange between case_when filter first full_join group_by left_join mutate rename select summarise
#' @importFrom magrittr %>%
#' @importFrom lubridate force_tz hour month year ymd
#' @importFrom odbc odbc odbcListDataSources
#' @importFrom DBI dbConnect dbReadTable dbDisconnect
#' @importFrom stringr str_c str_pad
#' @importFrom tidyr gather spread
#'
#' @description This function pulls in the spring and fall visit data and the
#' water level data from the NETN RAM database to create a table that is used by
#' the compile_sent_WL() function to calculate water level relative to the
#' wetland surface. Only works for a year at a time and for the growing season only.
#' Because well visits are staggered, the fall BARO reading may not exist for the
#' hour that the field water level measurement was taken. This function accounts
#' for that by taking the last logged BARO measurement and using that for the correction
#' factor. If the field measurement and the logged measurement are more than 2 hours apart,
#' you will receive a warning. \strong {This function was designed to work with the most recent
#' growing season of data to generate. Must have a the NETN RAM backend database
#' named "RAM_BE" as a DSN.} Function is primarily for internal use.
#'
#' @param path Quoted path of the folder where the output will be saved
#' @param year Numeric. The year you are preparing the data for. Function will only run
#' 1 year at a time.
#' @param visits both, spring, fall
#' \describe{
#' \item{"both"}{The default. If selected, the correction factor will be an average
#' of the spring and fall visit.}
#' \item{"spring"}{The correction factor will only be derived from the spring visit.}
#' \item{"fall"}{The correction factor will only be derived from the fall visit.
#' Use this option if wells were reset during the spring visit and had not acclimated.}
#' }
#'
#' @param export \code{TRUE} or \code{FALSE}. Export csv file to specified path.
#' Defaults to \code{TRUE}.
#'
#' @examples
#' # Create conversion table for fall-only data
#' dir = c('C:/Water_level_data/growing_season_2019')
#' conv_tbl_19 <- prep_conv_table(path = dir, year = 2019, visits = "fall", export = TRUE)
#'
#' # Create conversion table that averages spring and fall visit without
#' # printing messages in the console or saving output to file.
#' dir = c('C:/Water_level_data/growing_season_2019')
#' conv_tbl_19 <- prep_conv_table(path = dir, year = 2019, visits = "both",
#'                                export = FALSE, quietly = TRUE)
#'
#' @return Returns a data frame with visit times, site, ground height, and correction factor
#'
#' @export

prep_conv_table <- function(path = NA, year = 2019, visits = c('both', 'spring', 'fall'),
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
  if(dim(odbc::odbcListDataSources() %>% filter(name == "RAM_BE"))[1] == 0){
    stop('Compile function failed. There is no DSN named "RAM_BE".')}

  if(length(year) != 1){
    stop("Too many years specified. Function will only run for 1 year at a time.")
  }

db <- DBI::dbConnect(drv = odbc::odbc(), dsn="RAM_BE")
if(quietly == FALSE) {cat("Importing data tables from NETN RAM database.")}
assign("well_visit", DBI::dbReadTable(db, "tbl_Well_Visit"), envir = .GlobalEnv)
assign("well_loc", DBI::dbReadTable(db, "tbl_Well"), envir = .GlobalEnv)
DBI::dbDisconnect(db)
if(quietly == FALSE) {cat("..")}

west <- c("BIGH", "DUCK", "HEBR", "HODG", "WMTN")
east <- c("GILM", "LIHU", "NEMI")

# Take imported raw water level data for the year specified and make it wide
#well_prep <- prep_well_data(year = year, growing_season = FALSE, export = FALSE)
well_prep <- force(prep_well_data(year = year, growing_season = FALSE,
                                  export = FALSE, quietly = TRUE))

if(quietly == FALSE) {cat("Done.", sep = "\n")}

if(quietly == FALSE) {cat("Creating conversion table")}

# Convert raw wl pressure data and convert it to cm of water above the logger
well_prep2 <- well_prep %>% mutate(BIGH_cm = (BIGH_AbsPres-WMTN_BARO_AbsPres)*10.197,
                                   DUCK_cm = (DUCK_AbsPres-WMTN_BARO_AbsPres)*10.197,
                                   GILM_cm = (GILM_AbsPres-SHED_BARO_AbsPres)*10.197,
                                   HEBR_cm = (HEBR_AbsPres-WMTN_BARO_AbsPres)*10.197,
                                   HODG_cm = (HODG_AbsPres-WMTN_BARO_AbsPres)*10.197,
                                   LIHU_cm = (LIHU_AbsPres-SHED_BARO_AbsPres)*10.197,
                                   NEMI_cm = (NEMI_AbsPres-SHED_BARO_AbsPres)*10.197,
                                   WMTN_cm = (WMTN_AbsPres-WMTN_BARO_AbsPres)*10.197
                                   ) %>%
  select(timestamp, doy, BIGH_cm:WMTN_cm)

# Prepare well visit data
well_visit2 <- merge(well_loc[ , c("ID", "Site_Code", "Logger_Length", "MP_to_Bolt")], well_visit,
                     by.x = "ID", by.y = "Well_ID", all.x = FALSE, all.y = TRUE) %>%
  select(Site_Code, Visit_Date, Time, Stick_Up_at_MP, Logger_Length, MP_to_Bolt, Water_Depth, Water_Depth_Time)

#------------------------------------------------------------------------------
# Preparing data, so we can compare the water level measurement from the logger
# to the field WL measurement. Because several sites are checked after the
# WMTN_BARO logger is downloaded in the fall, we have to find the last non-NA
# measurement logged by the WMTN_BARO for those sites to compare with (last_logger_info).
#------------------------------------------------------------------------------

# Preparing visit data for spring and fall
well_visit3 <- well_visit2 %>% mutate(Year = lubridate::year(Visit_Date),
                                     month = lubridate::month(Visit_Date),
                                     season = case_when(between(month, 4, 6) ~ "spring",
                                                        between(month, 9, 11) ~ "fall",
                                                        TRUE ~ "offseason"),
                                     hour = lubridate::hour(Time),
                                     water_depth_time = stringr::str_c(ymd(Visit_Date), " ",
                                                       str_pad(hour, 2, side='left', pad = "0"),
                                                       ":00", sep = ""),
                                     ground = (Logger_Length + MP_to_Bolt) - Stick_Up_at_MP) %>%
                               filter(Year %in% year) %>%
                               select(Site_Code, Visit_Date, Year, month,
                                      water_depth_time, season, Water_Depth, ground, Stick_Up_at_MP)

# Fixing the time format
well_visit3$water_depth_time <- as.POSIXct(well_visit3$water_depth_time,
                                           format = "%Y-%m-%d %H:%M", tz = "America/New_York" )

# Check for offseason visits
if(dim(well_visit3 %>% filter(season %in% "offseason") %>% droplevels())[1]>0){
        cat(paste("Warning: There are well visits outside of spring or fall periods. ",
                  "Offseason measurements are omitted from this function unless visits == 'all'",
                  sep = "\n"))
  }


# prep raw water level for year of interest
raw_wl_yr <- raw_wl %>% mutate(logger_time = force_tz(Measure_Date_Time, tz = "America/New_York")) %>%
                        filter(year(logger_time) == year) %>% select(-ID) %>%
                        left_join(., well_loc[ , c("ID", "Site_Code")], by = c("Well_ID" = "ID"))

if(quietly == FALSE) {cat("....")}

#--------------------------
# Spring measurements
#--------------------------
spring_visit <- well_visit3 %>% filter(season == "spring") %>%
                                filter(!Site_Code %in% c("SHED_BARO", "WMTN_BARO"))

baro_log_times <- sort(unique(spring_visit$water_depth_time))


spr_wl_long <- well_prep2 %>% filter(timestamp %in% baro_log_times) %>%
                              gather("Site_Code1", "WL_cm", -timestamp, -doy) %>%
                              mutate(Site_Code = substr(Site_Code1, 1, 4)) %>%
                              select(-Site_Code1) #%>%

spring_meas <- left_join(spring_visit, spr_wl_long[ ,c("timestamp", "WL_cm", "Site_Code")],
                         by = c("water_depth_time" = "timestamp", "Site_Code" = "Site_Code"))


spring_meas2 <- spring_meas %>% mutate(spring_log_WL = WL_cm - (ground),
                                       field_WL = Stick_Up_at_MP - Water_Depth,
                                       corfac = spring_log_WL - field_WL,
                                       wellabs = paste(Site_Code, "AbsPres", sep = "_"),
                                       season = "spring") %>%
                                rename("visit_time" = "water_depth_time")

conv_tbl_spring <- spring_meas2 %>% select(visit_time, season, Site_Code, wellabs, ground, corfac) %>%
                                    mutate(spring_visit_time = visit_time,
                                           BARO_cor = case_when(Site_Code %in% east ~ "SHED_BARO",
                                                                Site_Code %in% west ~ "WMTN_BARO"))

spring_dates <- conv_tbl_spring %>% select(Site_Code, spring_visit_time)

#--------------------------
# Fall measurements
#--------------------------
# The WMTN_BARO logger is often downloaded before DUCK, HEBR, and HODG are downloaded.
# To compare the field WL measurements in those sites, with the logger WL, we need
# to add the ability to lag the logger water level data by up to 2 hours (not likely
# to change all that much in 2 hours).

well_prep_desc <- well_prep2 %>% mutate(timestamp = force_tz(timestamp, "America/New_York")) %>%
                                          arrange(desc(timestamp))

last_log_info <- function(site_col){
  site <- substr(site_col, 1, 4)
  df <- well_prep_desc %>% select(timestamp, site_col)
  df2 <- na.omit(df)[1,]
  df2$Site_Code <- site
  colnames(df2) <- c("last_log_time", "last_log_cm", "Site_Code")
  return(df2)
}

last_log_fall <- rbind(last_log_info("BIGH_cm"),
                       last_log_info("DUCK_cm"),
                       last_log_info("GILM_cm"),
                       last_log_info("HEBR_cm"),
                       last_log_info("HODG_cm"),
                       last_log_info("LIHU_cm"),
                       last_log_info("NEMI_cm"),
                       last_log_info("WMTN_cm"))

# pull out fall visit
fall_visit <- well_visit3 %>% filter(season == "fall" &
                                     !Site_Code %in% c("WMTN_BARO", "SHED_BARO")) %>%
                              droplevels()

# combine fall visit data with barometric logger data
fall_meas <- left_join(fall_visit, last_log_fall, by = c("Site_Code"))

fall_check <-fall_meas %>% mutate(time_diff = (water_depth_time - last_log_time)) %>%
                           filter(time_diff > 2)

if(quietly == FALSE & nrow(fall_check)>0) {cat("\n")}

if (nrow(fall_check)>0) {cat("Warning: There are", nrow(fall_check),
                             "sites with fall field water level measurements
                             more than 2 hours after the last logged value.")}

if(quietly == FALSE & nrow(fall_check)>0) {cat("\n")}

fall_meas2 <- fall_meas %>% mutate(last_log_WL = last_log_cm - (ground),
                                          field_WL = Stick_Up_at_MP - Water_Depth,
                                          corfac = last_log_WL - field_WL,
                                   wellabs = paste(Site_Code, "AbsPres", sep = "_"),
                                   season = "fall") %>%
  rename("visit_time" = "water_depth_time")

conv_tbl_fall1 <- fall_meas2 %>% select(visit_time, season, Site_Code, wellabs, ground, corfac)

conv_tbl_fall <- left_join(conv_tbl_fall1, spring_dates, by=c("Site_Code"))%>%
                 mutate(BARO_cor = case_when(Site_Code %in% east ~ "SHED_BARO",
                                             Site_Code %in% west ~ "WMTN_BARO"))

conv_table <- if(visits == "both"){
                  rbind(conv_tbl_spring, conv_tbl_fall) %>% group_by(Site_Code, wellabs) %>%
                        summarise(visit_time = last(visit_time),
                                  season = "both",
                                  ground = first(ground),
                                  corfac = mean(corfac, na.rm=TRUE),
                                  spring_visit_time = first(spring_visit_time),
                                  BARO_cor = first(BARO_cor))
              } else if (visits == "fall"){
                  conv_tbl_fall
              } else if (visits == "spring"){
                  conv_tbl_spring
              }

if(quietly == FALSE) {cat("Done.", sep = "\n")}

if(export == TRUE){
  filename <- paste0("conv_table_", year, ".csv")
  write.csv(conv_table, paste0(path, filename), row.names = FALSE)
  if(quietly == FALSE) {cat(paste0("File: ", filename, " saved to: ", "\n", "\t",
                                  path))}
}

return(conv_table)
}
