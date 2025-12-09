#' @title calc_WL_stats: Calculates growing season water level statistics.
#'
#' @importFrom dplyr between filter group_by lag left_join mutate n select summarise ungroup
#' @importFrom lubridate month
#' @importFrom tidyr gather spread
#' @importFrom purrr reduce
#'
#' @description This function calculates growing season water level statistics by site
#' and by year for sentinel wetlands in Acadia NP. Metrics follow Miller & Weed 2017
#'
#' @seealso To view report that metrics are based on: \url{https://irma.nps.gov/DataStore/Reference/Profile/2239739}
#'
#' @param df Data frame containing water level and precipitation data
#' @param from Year to start analysis, ranging from 2013-2019
#' @param to Year to stop analysis, ranging from 2013-2019
#'
#' @examples
#' \dontrun{
#' well_data <- read.csv("well_prec_data_2013-2019.csv")
#' gs_wl_stats <- calc_WL_stats(df = well_data, from = 2017, to = 2019)
#' }
#'
#' @return Returns a data frame with water level growing season statistics.
#'
#' @export

#------------------------
calc_WL_stats <- function(df, from = 2013, to = 2019){

EDT<-"America/New_York"
well_prp <- df |> mutate(timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M"),
                          month = lubridate::month(timestamp),
                          mon = months(timestamp, abbreviate = T)) |>
                   filter(doy > 134 & doy < 275) |> droplevels()

well_prp2 <- well_prp |> group_by(Year) |>
                          mutate(lag.precip = dplyr::lag(precip_cm, n = 1)) |>
                          ungroup()

well_prp_yr <- well_prp2 |> filter(between(Year, from, to)) |> droplevels()

# May 1 DOY= 121; May 15 = 135; Oct.1 = 274
well_prp_long <- well_prp_yr |> gather("site","water_level_cm",
                                   -timestamp, -Date, -doy, -Year, -hr,
                                   -doy_h, -month, -mon, -precip_cm, -lag.precip)

well_prp_long2 <- well_prp_long |> group_by(Year, site) |>
                                    mutate(lag_WL = dplyr::lag(water_level_cm, n = 1),
                                           change_WL = water_level_cm-lag_WL)

# Calculate growing season stats
well_gs_stats <- well_prp_long2 |> group_by(Year, site) |>
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
              WL_sd = sd(water_level_cm, na.rm = TRUE),
              WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
              WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
              max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
              max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
              prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100)

# Calculate change in WL from average Jun to average September
well_gs_month <- well_prp_long2 |> group_by(Year, mon, site) |>
                                    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE)) |>
                                    filter(mon %in% c("Jun","Sep")) |> droplevels() |> spread(mon, WL_mean) |>
                                    mutate(GS_change = Sep - Jun)


well_gs_prop1 <- well_prp_long2 |> mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                           bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                                 !is.na(water_level_cm), 1, 0),
                                           under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                           num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))

well_gs_prop <- well_gs_prop1 |> group_by(Year, site) |>
                                  summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
                                            prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
                                            prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100)

gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) |>
               reduce(left_join, by = c("Year", "site"))

# Missing water level data from 2017, change to NA
metrics<-c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
           "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )

gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics]<-NA
# Logger failed in DUCK in 2017

prop_complete_check <- length(gs_WL_stats$prop_GS_comp[gs_WL_stats$prop_GS_comp < 90])

if(prop_complete_check > 0) {
  message(paste0("Warning: There are ", prop_complete_check, " sites that have water level measurements for less than 90% of growing season."))
  }

return(gs_WL_stats)
}
