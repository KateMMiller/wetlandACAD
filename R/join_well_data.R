#' @title join_well_data: Joins sentinal well data using data files that are exported from HoboWare
#' as .csv files (for internal use).
#'
#' @importFrom dplyr select filter mutate full_join
#' @importFrom magrittr %>%
#' @importFrom purrr reduce
#' @importFrom lubridate yday
#'
#' @description This function finds the well data files based on a specified path
#' and partial matching of files based on site name, reads these files into R,
#' adds Well_ID and site name to each file, then joins the data to create a wide
#' format of the data with the timestamp as the joining column. Function requires
#' all 8 sites and 2 barometric loggers to run. Mostly for internal use.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#'
#' @examples
#' path='D:/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Fall_2018'
#' well_data<-join_well_data(path)
#'
#' @return Returns a wide data frame with timestamp, SITENAME_AbsPres, SITENAME_C.
#'
#' @export
#'

join_well_data<-function(path, output=c('short', 'verbose')){

  output<-match.arg(output)

  path<-if(substr(path,nchar(path),nchar(path))!="/"){paste0(path,"/")} else(paste0(path))

  filenames<-list.files(path = path, pattern =".csv")

  duck<-read.table(paste0(path,filenames[grep('Duck',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','DUCK_AbsPres','DUCK_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  lihu<-read.table(paste0(path,filenames[grep('LittleHunter',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','LIHU_AbsPres','LIHU_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  gilm<-read.table(paste0(path,filenames[grep('Gilmore',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','GILM_AbsPres','GILM_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  wmtn<-read.table(paste0(path,filenames[grep('WMTN_well',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','WMTN_AbsPres','WMTN_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  hodg<-read.table(paste0(path,filenames[grep('Hodgdon',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','HODG_AbsPres','HODG_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  nemi<-read.table(paste0(path,filenames[grep('NewMills',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','NEMI_AbsPres','NEMI_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  hebr<-read.table(paste0(path,filenames[grep('Heath_Brook',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','HEBR_AbsPres','HEBR_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  bigh<-read.table(paste0(path,filenames[grep('Big_Heath',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','BIGH_AbsPres','BIGH_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  wmtn_baro<-read.table(paste0(path,filenames[grep('westmtnswmp',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                        col.names=c('V1','Measure_Date_Time','WMTN_BARO_AbsPres','WMTN_BARO_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  shed_baro<-read.table(paste0(path,filenames[grep('shed',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                        col.names=c('V1','Measure_Date_Time','SHED_BARO_AbsPres','SHED_BARO_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  duck <- duck %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          DUCK_AbsPres=as.numeric(DUCK_AbsPres), DUCK_C=as.numeric(DUCK_C)) %>% select(-Measure_Date_Time)

  lihu <- lihu %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          LIHU_AbsPres=as.numeric(LIHU_AbsPres), LIHU_C=as.numeric(LIHU_C)) %>% select(-Measure_Date_Time)

  gilm <- gilm %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          GILM_AbsPres=as.numeric(GILM_AbsPres), GILM_C=as.numeric(GILM_C)) %>% select(-Measure_Date_Time)

  wmtn <- wmtn %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          WMTN_AbsPres=as.numeric(WMTN_AbsPres), WMTN_C=as.numeric(WMTN_C)) %>% select(-Measure_Date_Time)

  hodg <- hodg %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          HODG_AbsPres=as.numeric(HODG_AbsPres), HODG_C=as.numeric(HODG_C)) %>% select(-Measure_Date_Time)

  nemi <- nemi %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          NEMI_AbsPres=as.numeric(NEMI_AbsPres), NEMI_C=as.numeric(NEMI_C)) %>% select(-Measure_Date_Time)

  hebr <- hebr %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          HEBR_AbsPres=as.numeric(HEBR_AbsPres), HEBR_C=as.numeric(HEBR_C)) %>% select(-Measure_Date_Time)

  bigh <- bigh %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                          timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                          BIGH_AbsPres=as.numeric(BIGH_AbsPres), BIGH_C=as.numeric(BIGH_C)) %>% select(-Measure_Date_Time)

  wmtn_baro <- wmtn_baro %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                                    timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                                    WMTN_BARO_AbsPres=as.numeric(WMTN_BARO_AbsPres), WMTN_BARO_C=as.numeric(WMTN_BARO_C)) %>%
    select(-Measure_Date_Time)

  shed_baro <- shed_baro %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %I:%M:%S %p", tz="America/New_York"),
                                    timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S", tz="America/New_York"),
                                    SHED_BARO_AbsPres=as.numeric(SHED_BARO_AbsPres), SHED_BARO_C=as.numeric(SHED_BARO_C)) %>%
    select(-Measure_Date_Time)

  combdata<-list(wmtn_baro, shed_baro, duck, lihu, gilm, wmtn, hodg, nemi, hebr, bigh) %>% reduce(full_join, by='timestamp')
  combdata<-combdata %>% mutate(doy=yday(timestamp))

  combdata1<-if (output=='short'){combdata %>% select(timestamp, doy, WMTN_BARO_AbsPres, SHED_BARO_AbsPres, BIGH_AbsPres,
                                                     DUCK_AbsPres, GILM_AbsPres, HEBR_AbsPres, HODG_AbsPres,
                                                    LIHU_AbsPres, NEMI_AbsPres, WMTN_AbsPres)
             } else if (output=='verbose') {combdata %>% select(timestamp, doy, WMTN_BARO_AbsPres, SHED_BARO_AbsPres,
                                                                BIGH_AbsPres, DUCK_AbsPres, GILM_AbsPres, HEBR_AbsPres,
                                                                HODG_AbsPres, LIHU_AbsPres, NEMI_AbsPres, WMTN_AbsPres,
                                                               WMTN_BARO_C, SHED_BARO_C, BIGH_C, DUCK_C, GILM_C, HEBR_C,
                                                                HODG_C, LIHU_C, NEMI_C,WMTN_C
               )}

  return(combdata1)
}
