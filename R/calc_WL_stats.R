#' @title calc_WL_stats: Calculates growing season water level statistics.
#'
#' @importFrom dplyr filter group_by lag left_join mutate n select summarise ungroup
#' @importFrom lubridate month
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr reduce
#'
#' @description This function calculates growing season water level statistics by site
#' and by year for sentinel wetlands in Acadia NP. Metrics follow Miller & Weed 2017.
#'
#' @seealso To view report that metrics are based on: \url{https://irma.nps.gov/DataStore/Reference/Profile/2239739}
#'
#' @param df Data frame containing water level and precipitation data
#' @param years Year to start analysis, ranging from 2013 to current year
#'
#' @examples
#' \dontrun{
#' well_data <- read.csv("../data/well_prec_data_2013-2025.csv")
#' gs_wl_stats <- calc_WL_stats(df = well_data, years = 2013:2025)
#' }
#'
#' @return Returns a data frame with water level growing season statistics.
#'
#' @export

#------------------------
calc_WL_stats <- function(df, years = 2013:as.numeric(format(Sys.Date(), "%Y"))){

EDT <-"America/New_York"
# ENDED HERE
df$timestamp <- as.POSIXct(ifelse(df$hr == 0, paste0(df$timestamp, " 00:00:00"), df$timestamp),
                           format = "%Y-%m-%d %H:%M:%S")

well_prp <- df |> mutate(month = lubridate::month(timestamp),
                         mon = months(timestamp, abbreviate = T)) |>
                  filter(doy > 134 & doy < 275) |> droplevels()

well_prp2 <- well_prp |> group_by(Year) |>
                         mutate(lag.precip = dplyr::lag(precip_cm, n = 1)) |>
                         ungroup()

well_prp_yr <- well_prp2 |> filter(Year %in% years) |> droplevels()

# May 1 DOY= 121; May 15 = 135; Oct.1 = 274
well_prp_long <- well_prp_yr |> pivot_longer(cols = c(contains("_WL")),
                                             names_to = "site", values_to = "water_level_cm")

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
              prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100,
              .groups = "drop")

# Calculate change in WL from average Jun to average September
well_gs_month <- well_prp_long2 |> group_by(Year, mon, site) |>
                                   summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
                                             .groups = 'drop') |>
                                   filter(mon %in% c("Jun","Sep")) |> droplevels() |>
                                   #spread(mon, WL_mean) |>
                                   pivot_wider(names_from = mon, values_from = WL_mean) |>
                                   mutate(GS_change = Sep - Jun)


well_gs_prop1 <- well_prp_long2 |> mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                          bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                               !is.na(water_level_cm), 1, 0),
                                          under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                          num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))

well_gs_prop <- well_gs_prop1 |> group_by(Year, site) |>
                                  summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/
                                                               sum(num_logs, na.rm = TRUE))*100,
                                            prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/
                                                                    sum(num_logs, na.rm = TRUE))*100,
                                            prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/
                                                                    sum(num_logs, na.rm = TRUE))*100,
                                            .groups = 'drop')

gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) |>
                    reduce(left_join, by = c("Year", "site"))

# Missing water level data from 2017, change to NA
metrics <- c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )

#gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics] <- NA
# Logger failed in DUCK in 2017

prop_complete_check <- gs_WL_stats[is.na(gs_WL_stats$prop_GS_comp) | gs_WL_stats$prop_GS_comp < 90,]

if(nrow(prop_complete_check) > 0) {
  message(paste0("Warning: The following ", nrow(prop_complete_check),
                 " sites have water level measurements for less than 90% of growing season: ",
                 "\n", "site    ", "year ", "%_complete", "\n",
                 paste(prop_complete_check$site, prop_complete_check$Year,
                       round(prop_complete_check$prop_GS_comp, 2), collapse = "\n", sep = " ")))
  }

return(gs_WL_stats)
}
