#' @title conv_kpa_cm: converts absolute pressure to cm of water above the logger
#'
#' @description This function converts absolute pressure in kPa to cm of water. For this
#' function to work, you must have your pressure field named: SITECODE_AbsPres. The SITECODE
#' then needs to be defined in the function arguments.
#'
#' @param df data frame with pressure data
#' @param site_code Quoted code for site name that corresponds to a field in the data frame
#' @param baro Quoted field name of the atmostpheric barometric pressure correction field
#'
#' @examples
#' df <- data.frame(BIGH_AbsPres = c(100, 99, 98), BARO_AbsPres = c(99, 96, 94))
#' df2 <- conv_kpa_cm(df, site_code = 'BIGH', baro = 'BARO_AbsPres')
#'
#' @return Returns a data frame with a new column labeled with SITECODE_cm, which represents
#' cm of water above the logger.
#'
#' @export

conv_kpa_cm<-function(df, site_code, baro){
  pres<-paste0(site_code, '_AbsPres')
  if(!is.numeric(df[,pres]) | !is.numeric(df[,baro])) stop('fields must be numeric')
  df$cm<-NA
  df$cm<-ifelse(!(is.na(df[,pres]) & is.na(df[,baro])),(df[,pres]-df[,baro])*10.197, NA)
  names(df)[names(df)=='cm']<-paste0(site_code,'_cm')
  return(df)
}
