#' @title get_NADP_precip: Downloads and cleans hourly precipitation data from NADP
#'
#' @importFrom XML readHTMLTable
#' @importFrom dplyr select filter mutate
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#' @importFrom lubridate date year
#'
#' @description This function uses start date, end date and stationID to download NADP
#' hourly precipitation data, and converts preciptation from inches to cm. Note that
#' some stations may have different download links, and may not always work. The link
#' used to download the NADP data was generated for station: ME98.
#'
#' @param start_date Quoted date for first day of period (eg "04/01/2018")
#' @param end_date Quoted date for last day of period (eg "10/31/2018")
#' @param stationID Quoted name of station to pull data from (eg "ME98" is ACAD's weather station)
#' @param quietly \code{TRUE} or \code{FALSE}. If \code{FALSE}, code will not print progress into console.
#' Defaults to \code{FALSE}
#'
#' @examples
#' precip_2018<- get_NADP_precip(start_date='04/01/2018', end_date='10/31/2018', stationID='ME98')
#'
#' @return Returns a data frame with Date, hour (0-23), timestamp (Y-M-D H:M:S), and precip_cm
#'
#' @export
#'

get_NADP_precip <- function(start_date = "04/01/2018", end_date = "10/31/2018", stationID = "ME98", quietly = FALSE){

  if(quietly == FALSE) {cat("Downloading NADP precip data")}

  PrecipURL<-paste0('http://nadp.slh.wisc.edu/siteOps/ppt/Data.aspx?id=',stationID,'&stdate=',
                    start_date,'T13:35&endate=', end_date,
                    'T13:30&plot_sel=1111110&data_sel1=H&data_sel2=110&sel2_count=2&offset_txt=-5')

  if(quietly == FALSE) {cat("....")}

  precip_tbl <- readHTMLTable(PrecipURL, header=T, as.data.frame = T,
                              stringsAsFactors = F)$GridView_data[,c('Date','Hour','Precip (in)')]

  if(is.null(precip_tbl)){
    stop("Function returned dataframe with 0 records. Check NADP website to ensure the date range is available and station ID exists.")}

  if(quietly == FALSE) {cat("Done.", sep = "\n")}

  if(quietly == FALSE) {cat("Formatting precip data")}

  precip_tbl2<-precip_tbl %>% filter(Date!='Totals:') %>% droplevels() %>%
                              mutate(hour = str_sub(Hour,1,2),
                                     timestamp = as.POSIXct(paste0(Date, " ", hour, ":00:00"),
                                       format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York"),
                                     Date = lubridate::date(timestamp),
                                     Year = lubridate::year(timestamp),
                                     precip_in = as.numeric(`Precip (in)`), precip_cm = precip_in  *2.54) %>%
                              select(timestamp, Date, Year, hour, precip_cm)

  if(quietly == FALSE) {cat("....Done.")}

  return(precip_tbl2)}
