#' @include conv_kpa_cm.R
#' @include conv_WL.R
#'
#' @title compile_sent_WL: Calculates water level from raw sentinal well data
#'
#' @description This function uses (and requires) the output from join_well_data() to
#' calculate the water level relative to the wetland surface for each sentinal site.
#' This function also requires a data frame called conv_table with the following fields:
#'   site, wellabs, ground, corfac, baro
#'
#' @param df data frame output from join_well_data
#' @param conv_table data frame listing site and well info for WL conversion
#'
#' @examples
#' path='D:/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Fall_2018'
#' well_data<-join_well_data(path)
#' conv_table<-data.frame(site=c('BIGH','DUCK','GILM','HEBR','HODG','LIHU','NEMI','WMTN'),
#'                        wellabs=c("BIGH_AbsPres", "DUCK_AbsPres", "GILM_AbsPres", "HEBR_AbsPres",
#'                                  "HODG_AbsPres", "LIHU_AbsPres", "NEMI_AbsPres", "WMTN_AbsPres"),
#'                        ground=c(88.9, 79.4, 63.1, 71.8, 95.1, 73.2, 85.7, 68.5),
#'                        corfac=c(5.004, 4.73, 18.73, 0.845, 7.199, 26.566, 16.556, 7.284),
#'                        baro=c('WMTN_BARO_AbsPres', 'WMTN_BARO_AbsPres', 'SHED_BARO_AbsPres', 'WMTN_BARO_AbsPres',
#'                               'WMTN_BARO_AbsPres', 'SHED_BARO_AbsPres', 'SHED_BARO_AbsPres', 'WMTN_BARO_AbsPres'))
#' df<-comb_sent_WL(well_data,conv_table)
#'
#' @return Returns a data frame with the water level relative to the ground surface for each
#' sentinal water level logger.
#'
#' @export

compile_sent_WL<-function(df, conv_table){
  conv_table=conv_table
  df<-conv_kpa_cm(df, pres=as.character(conv_table[1, 2]),baro=as.character(conv_table[1, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[2, 2]),baro=as.character(conv_table[2, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[3, 2]),baro=as.character(conv_table[3, 5]))

  df<-conv_kpa_cm(df, pres=as.character(conv_table[4, 2]),baro=as.character(conv_table[4, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[5, 2]),baro=as.character(conv_table[5, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[6, 2]),baro=as.character(conv_table[6, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[7, 2]),baro=as.character(conv_table[7, 5]))
  df<-conv_kpa_cm(df, pres=as.character(conv_table[8, 2]),baro=as.character(conv_table[8, 5]))

  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[1, 1],"_cm")),
              ground=conv_table[1, 3], corfac=conv_table[1,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[2, 1],"_cm")),
              ground=conv_table[2, 3], corfac=conv_table[2,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[3, 1],"_cm")),
              ground=conv_table[3, 3], corfac=conv_table[3,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[4, 1],"_cm")),
              ground=conv_table[4, 3], corfac=conv_table[4,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[5, 1],"_cm")),
              ground=conv_table[5, 3], corfac=conv_table[5,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[6, 1],"_cm")),
              ground=conv_table[6, 3], corfac=conv_table[6,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[7, 1],"_cm")),
              ground=conv_table[7, 3], corfac=conv_table[7,4])
  df<-conv_WL(df, cm_water=as.character(paste0(conv_table[8, 1],"_cm")),
              ground=conv_table[8, 3], corfac=conv_table[8,4])
  df<-df[,c('timestamp','doy','BIGH_WL','DUCK_WL','GILM_WL','HEBR_WL','HODG_WL','LIHU_WL','NEMI_WL','WMTN_WL')]
  # I know this function is fugly- just can't figure out how to iterate through each site
  # for both the conv_kpa_cm and conv_WL functions with mapply.
  }
