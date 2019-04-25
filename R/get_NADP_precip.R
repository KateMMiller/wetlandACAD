#' @title get_NADP_precip: Downloads and cleans hourly precipitation data from NADP
#'
#' @importFrom XML readHTMLTable
#' @importFrom dplyr select filter mutate
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#'
#' @description This function uses start date, end date and stationID to download NADP
#' hourly precipitation data, and converts preciptation from inches to cm. Note that
#' some stations may have different download links, and may not always work. The link
#' used to download the NADP data was generated for station: ME98.
#'
#' @param start_date Quoted date for first day of period (eg "04/01/2018")
#' @param end_date Quoted date for last day of period (eg "10/31/2018")
#' @param stationID Quoted name of station to pull data from (eg "ME98" is ACAD's weather station)
#'
#' @examples
#' precip_2018<- get_NADP_precip(start_date='04/01/2018', end_date='10/31/2018', stationID='ME98')
#'
#' @return Returns a data frame with Date, hour (0-23), timestamp (Y-M-D H:M:S), and precip_cm
#'
#' @export
#'

get_NADP_precip<-function(start_date="04/01/2018", end_date="10/31/2018", stationID="ME98"){

  PrecipURL<-paste0('http://nadp.slh.wisc.edu/siteOps/ppt/Data.aspx?id=',stationID,'&stdate=',
                    start_date,'T13:35&endate=', end_date,
                    'T13:30&plot_sel=1111110&data_sel1=H&data_sel2=110&sel2_count=2&offset_txt=-5')

  precip_tbl<-readHTMLTable(PrecipURL,header=T, as.data.frame = T, stringsAsFactors=F)$GridView_data[,c('Date','Hour','Precip (in)')]

  precip_tbl2<-precip_tbl %>% filter(Date!='Totals:') %>% droplevels() %>%
    mutate(hour=str_sub(Hour,1,2),
           timestamp=as.POSIXct(paste0(Date," ", hour, ":00:00"), format="%Y-%m-%d %H:%M:%S", tz="America/New_York"),
           precip_in=as.numeric(`Precip (in)`), precip_cm=precip_in*2.54) %>%
    select(-`Precip (in)`,-precip_in, -Hour)

  return(precip_tbl2)}
