#' @title conv_WL: converts cm of water above the logger to water level relative to surface
#'
#' @description This function converts cm of water above the logger to water level relative
#' to the surface of the weltand. For this function to work, you must have a field named: SITECODE_cm.
#' To get this field, should either run the conv_kpa_cm() function first, or calculate cm of water
#' for that site by hand. The SITECODE also needs to be defined in the function arguments.
#'
#' @param df data frame with pressure data
#' @param cm_water Quoted field name containing cm of water above logger
#' @param ground numeric distance between the logger and the surface of the wetland
#' (Logger Length + Measuring Point to Bold - Stick Up Height).
#' @param corfac numeric correction factor (in cm) to correct for difference in atmospheric pressure
#' due to differing elevations between where the water level loggers are and where the
#' atmospheric pressure loggers are.
#'
#' @examples
#' df <- data.frame(BIGH_AbsPres = c(110.1, 111.3, 110.4), BARO_AbsPres = c(102.1, 103.1, 102.8))
#' df <- conv_kpa_cm(df, pres = 'BIGH_AbsPres', baro = 'BARO_AbsPres')
#' df <- conv_WL(df, cm_water = 'BIGH_cm', ground = 88.9, cor = 5.004)
#'
#' @return Returns a data frame with a new column labeled with SITECODE_WL, which is the water level
#' in cm relative to the surface of the wetland. Negative values are below ground, positive are above ground.
#'
#' @export

conv_WL<-function(df, cm_water, ground, corfac){
  site_code<-substr(cm_water,1,4)
  df$WL<-NA
  df$WL<-ifelse(!(is.na(df[,cm_water])), df[,cm_water]-(ground+corfac), NA)
  names(df)[names(df)=='WL']<-paste0(site_code,'_WL')
  return(df)
}
