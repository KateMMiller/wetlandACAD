#' @title calc_WL_stats: Calculates growing season water level statistics.
#'
#' @importFrom dplyr mutate select group_by filter summarise lag left_join between
#' @importFrom magrittr %>%
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
#' well_data<-read.csv("well_prec_data_2013-2019.csv")
#' gs_wl_stats<-calc_WL_stats(df=well_data, from = 2017, to = 2019)
#'
#' @return Returns a data frame with water level growing season statistics.
#'
#' @export

#------------------------
calc_WL_stats<-function(df, from = 2013, to = 2019){

EDT<-"America/New_York"
well_prp<- df %>% mutate(
   timestamp= as.POSIXct(timestamp, format= "%m/%d/%Y %H:%M"),
   month = month(timestamp),
   mon = months(timestamp,abbr=T)) %>%
  filter(doy>134 & doy<275) %>% droplevels()

well_prp_yr<- well_prp %>% filter(between(year, from, to)) %>% droplevels()

# May 1 DOY= 121; May 15=135; Oct.1=274
well_prp_long<-well_prp_yr %>% gather("site","water_level_cm",
                                   -timestamp,-Date,-doy,-year,-hr,
                                   -doy_h,-month,-mon,-precip_cm,-lag.precip)
well_prp_long2<-well_prp_long %>% group_by(year,site) %>%
  mutate(lag_WL= lag(water_level_cm), change_WL=water_level_cm-lag_WL)

# Calculate growing season stats
well_gs_stats<-well_prp_long2 %>% group_by(year,site) %>%
  summarise(WL_mean = mean(water_level_cm, na.rm=T),
            WL_sd = sd(water_level_cm, na.rm=T),
            WL_min = suppressWarnings(min(water_level_cm, na.rm=T)),
            WL_max = suppressWarnings(max(water_level_cm, na.rm=T)),
            max_inc = suppressWarnings(max(change_WL, na.rm=T)),
            max_dec = suppressWarnings(min(change_WL, na.rm=T)))

# Calculate change in WL from average Jun to average September
well_gs_month<-well_prp_long2 %>% group_by(year,mon,site) %>%
  summarise(WL_mean = mean(water_level_cm, na.rm=T)) %>%
  filter(mon %in% c("Jun","Sep")) %>% droplevels() %>% spread(mon,WL_mean) %>%
  mutate(GS_change= Sep-Jun)


well_gs_prop1<-well_prp_long2 %>% mutate(over_0 = ifelse(water_level_cm>=0 & !is.na(water_level_cm), 1, 0),
                                        bet_0_neg30 = ifelse(water_level_cm<=0 & water_level_cm>= -30
                                                         & !is.na(water_level_cm), 1, 0),
                                        under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                        num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))

well_gs_prop<-well_gs_prop1 %>% group_by(year,site) %>%
  summarise(prop_over_0cm=(sum(over_0, na.rm=T)/sum(num_logs, na.rm=T))*100,
            prop_bet_0_neg30cm=(sum(bet_0_neg30, na.rm=T)/sum(num_logs, na.rm=T))*100,
            prop_under_neg30cm=(sum(under_neg30, na.rm=T)/sum(num_logs, na.rm=T))*100)

gs_WL_stats<-list(well_gs_stats, well_gs_month[,c("year","site","GS_change")], well_gs_prop) %>%
  reduce(left_join, by = c("year","site"))

# Missing water level data from 2017, change to NA
metrics<-c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "GS_change",
           "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )

gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$year==2017,metrics]<-NA

return(gs_WL_stats)
}
