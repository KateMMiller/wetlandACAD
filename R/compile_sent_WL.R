#' @include conv_kpa_cm.R
#' @include conv_WL.R
#'
#' @title compile_sent_WL: Calculates water level from raw sentinal well data.
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#'
#' @description This function uses (and requires) the output from prep_well_data() to
#' calculate the water level relative to the wetland surface for each sentinel site.
#' This should only be used for one year at a time, to ensure that the function is
#' using the correct well measurements to calculate water level. This function
#' also requires a data frame called conv_table with the following fields:
#'   site, wellabs, ground, corfac, baro
#'
#' @param df data frame output from join_well_data
#' @param conv_table data frame listing site and well info for WL conversion
#' @param between_visits \code{TRUE} or \code{FALSE}. If \code{TRUE} will include only measurements
#' between the spring and fall well visits. If \code{FALSE}, will return all measurements included
#' in the data frame generated from prep_well_data(). Defaults to \code{TRUE}.
#'
#' @examples
#'
#' # Compile tables
#' dir = c('C:/Water_level_data/growing_season_2019')
#' well19 <- prep_well_data(path = dir, year = 2019, growing_season = TRUE, export = FALSE)
#' conv_tbl_19 <- prep_conv_table(path = dir, year = 2019, visits = "fall", export = FALSE)
#'
#' # Create data frame with water level relative to surface, cut at spring well visit
#' well19WL<-compile_sent_WL(well19, conv_tbl_19, between_visits = TRUE)
#'
#' @return Returns a data frame with the water level relative to the ground surface for each
#' sentinal water level logger.
#'
#' @export
#'

compile_sent_WL<-function(df, conv_table, between_visits = TRUE){

  conv_table=conv_table
  df<-conv_kpa_cm(df, pres = as.character(conv_table[1, "wellabs"]),
                      baro = as.character(conv_table[1, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[2, "wellabs"]),
                      baro = as.character(conv_table[2, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[3, "wellabs"]),
                      baro = as.character(conv_table[3, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[4, "wellabs"]),
                      baro = as.character(conv_table[4, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[5, "wellabs"]),
                      baro = as.character(conv_table[5, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[6, "wellabs"]),
                      baro = as.character(conv_table[6, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[7, "wellabs"]),
                      baro = as.character(conv_table[7, "BARO_cor"]))
  df<-conv_kpa_cm(df, pres = as.character(conv_table[8, "wellabs"]),
                      baro = as.character(conv_table[8, "BARO_cor"]))

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[1, "Site_Code"],"_cm")),
                    ground = conv_tbl_19[1, "ground"],
                    corfac = conv_table[1, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[2, "Site_Code"],"_cm")),
                ground = conv_tbl_19[2, "ground"],
                corfac = conv_table[2, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[3, "Site_Code"],"_cm")),
                ground = conv_tbl_19[3, "ground"],
                corfac = conv_table[3, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[4, "Site_Code"],"_cm")),
                ground = conv_tbl_19[4, "ground"],
                corfac = conv_table[4, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[5, "Site_Code"],"_cm")),
                ground = conv_tbl_19[5, "ground"],
                corfac = conv_table[5, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[6, "Site_Code"],"_cm")),
                ground = conv_tbl_19[6, "ground"],
                corfac = conv_table[6, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[7, "Site_Code"],"_cm")),
                ground = conv_tbl_19[7, "ground"],
                corfac = conv_table[7, "corfac"])

  df <- conv_WL(df, cm_water = as.character(paste0(conv_table[8, "Site_Code"],"_cm")),
                ground = conv_tbl_19[8, "ground"],
                corfac = conv_table[8, "corfac"])

  df<-df[,c('timestamp','doy','BIGH_WL','DUCK_WL','GILM_WL','HEBR_WL','HODG_WL','LIHU_WL','NEMI_WL','WMTN_WL')]

  df2 <- if(between_visits == TRUE){
          df %>% mutate(BIGH_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "BIGH"], NA, BIGH_WL),
                        DUCK_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "DUCK"], NA, DUCK_WL),
                        GILM_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "GILM"], NA, GILM_WL),
                        HEBR_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "HEBR"], NA, HEBR_WL),
                        HODG_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "HODG"], NA, HODG_WL),
                        LIHU_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "LIHU"], NA, LIHU_WL),
                        NEMI_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "NEMI"], NA, NEMI_WL),
                        WMTN_WL = ifelse(timestamp < conv_tbl_19$spring_visit_time[conv_tbl_19$Site_Code == "WMTN"], NA, WMTN_WL))
  } else {df}

  return(df2)

  }
