#' @title conv_kpa_cm: converts absolute pressure to cm of water above the logger
#'
#' @importFrom dplyr mutate
#'
#' @description This function converts absolute pressure in kPa to cm of water. Has
#' been updated to work with new logger data.
#'
#' @param df data frame with pressure data
#' @param pres Quoted field name that corresponds to the differential pressure from
#' a water level logger. The differential pressure being the Absolute - Barometric,
#' which the new loggers calculate automatically.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(BIGH_AbsPres = c(100, 99, 98), WMTN_BARO_AbsPres = c(99, 96, 94))
#' df2 <- conv_kpa_cm(df, pres = 'BIGH_AbsPres', baro = 'WMTN_BARO_AbsPres')
#'
#' }
#'
#' @return Returns a data frame with a new column labeled with SITECODE_cm, which represents
#' cm of water above the logger.
#'
#' @export

conv_kpa_cm<-function(df, pres){
  df <- data.frame(df)

  col1 <- df[ , pres]

  df[, paste0(substr(pres,1,4), "_cm")] <- as.numeric(col1*10.197)

  return(df)
}
