#' @title conv_kpa_cm: converts absolute pressure to cm of water above the logger
#'
#' @description This function converts absolute pressure in kPa to cm of water.
#'
#' @param df data frame with pressure data
#' @param pres Quoted field name that corresponds absolute pressure from a water level logger
#' @param baro Quoted field name of the atmostpheric barometric pressure correction field
#'
#' @examples
#' df <- data.frame(BIGH_AbsPres = c(100, 99, 98), BARO_AbsPres = c(99, 96, 94))
#' df2 <- conv_kpa_cm(df, pres = 'BIGH_AbsPres', baro = 'BARO_AbsPres')
#'
#' @return Returns a data frame with a new column labeled with SITECODE_cm, which represents
#' cm of water above the logger.
#'
#' @export

conv_kpa_cm<-function(df, pres, baro){
  site_code<-substr(pres, 1, 4)
  df$cm<-NA
  df$cm<-ifelse(!(is.na(df[,pres]) & is.na(df[,baro])),(df[,pres]-df[,baro])*10.197, NA)
  names(df)[names(df)=='cm']<-paste0(site_code,'_cm')
  return(df)
}
