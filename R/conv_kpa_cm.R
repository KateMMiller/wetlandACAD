#' @title conv_kpa_cm: converts absolute pressure to cm of water above the logger
#'
#' @importFrom dplyr mutate
#'
#' @description This function converts absolute pressure in kPa to cm of water.
#'
#' @param df data frame with pressure data
#' @param pres Quoted field name that corresponds absolute pressure from a water level logger
#' @param baro Quoted field name of the atmostpheric barometric pressure correction field
#'
#' @examples
#' df <- data.frame(BIGH_AbsPres = c(100, 99, 98), WMTN_BARO_AbsPres = c(99, 96, 94))
#' df2 <- conv_kpa_cm(df, pres = 'BIGH_AbsPres', baro = 'WMTN_BARO_AbsPres')
#'
#' @return Returns a data frame with a new column labeled with SITECODE_cm, which represents
#' cm of water above the logger.
#'
#' @export

conv_kpa_cm<-function(df, pres, baro){
  df <- data.frame(df)

  col1 <- df[ , pres]
  col2 <- df[ , baro]

  df[, paste0(substr(pres,1,4), "_cm")] <- as.numeric((col1 - col2)*10.197)

  return(df)
}
