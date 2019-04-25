#' @title bind_well_data: Binds well data using data files that are exported from
#' HoboWare as .csv files.
#'
#' @description This function finds the well data files based on a specified path
#' and partial matching of files based on site name, reads these files into R,
#' adds Well_ID and site name to each file, then binds them together and prepares
#' the data to be imported into the MS Access RAM backend table: tbl_Water_Level.
#' If appending to the Access table is not needed, then drop the first column 'ID',
#' which is left blank so that Access can assign an auto number for the primary key.
#' This function is mostly for internal use to generate a long format of the data
#' to append to the MS Access table.
#'
#' @param path Quoted path of the folder where the exported Hobo tables are located.
#'
#' @examples
#' path='D:/NETN/Monitoring_Projects/Freshwater_Wetland/Hobo_Data/Fall_2018'
#' well_data<-bind_well_data(path)
#'
#' @return Returns a long data frame with ID (blank for Access to assign autonumber), Well_ID,
#' timestamp, Absolute_Pressure_kPa, Degrees_C, and site.
#'
#' @export

bind_well_data<-function(path){
  path<-if(substr(path,nchar(path),nchar(path))!="/"){paste0(path,"/")} else(paste0(path))

  filenames<-list.files(path = path, pattern =".csv")

  duck<-read.table(paste0(path,filenames[grep('Duck',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  lihu<-read.table(paste0(path,filenames[grep('LittleHunter',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  gilm<-read.table(paste0(path,filenames[grep('Gilmore',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  wmtn<-read.table(paste0(path,filenames[grep('WMTN_well',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  hodg<-read.table(paste0(path,filenames[grep('Hodgdon',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  nemi<-read.table(paste0(path,filenames[grep('NewMills',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  hebr<-read.table(paste0(path,filenames[grep('Heath_Brook',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  bigh<-read.table(paste0(path,filenames[grep('Big_Heath',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                   col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  wmtn_baro<-read.table(paste0(path,filenames[grep('westmtnswmp',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                        col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  shed_baro<-read.table(paste0(path,filenames[grep('shed',filenames)]), skip=1, sep=',', stringsAsFactors = FALSE,
                        col.names=c('V1','Measure_Date_Time','Absolute_Pressure_kPa','Degrees_C', 'V5','V6','V7','V8','V9'))[-1,2:4]

  duck <- duck %>% mutate(site = 'duck', Well_ID = 9, ID = NA)
  lihu <- lihu %>% mutate(site = 'lihu', Well_ID = 13, ID = NA)
  gilm <- gilm %>% mutate(site = 'gilm', Well_ID = 12, ID = NA)
  wmtn <- wmtn %>% mutate(site = 'wmtn', Well_ID = 10, ID = NA)
  hodg <- hodg %>% mutate(site = 'hodg', Well_ID = 16, ID = NA)
  nemi <- nemi %>% mutate(site = 'nemi', Well_ID = 14, ID = NA)
  hebr <- hebr %>% mutate(site = 'hebr', Well_ID = 15, ID = NA)
  bigh <- bigh %>% mutate(site = 'bigh', Well_ID = 11, ID = NA)
  wmtn_baro <- wmtn_baro %>% mutate(site = 'wmtn_baro', Well_ID = 17, ID = NA)
  shed_baro <- shed_baro %>% mutate(site = 'shed_baro', Well_ID = 19, ID = NA)

  combdata<-rbind(duck, lihu, gilm, wmtn, hodg, nemi, hebr, bigh, wmtn_baro, shed_baro)

  combdata<-combdata %>% mutate(Measure_Date_Time=as.POSIXct(Measure_Date_Time, format="%m/%d/%y %H:%M:%S"),
                                timestamp= as.POSIXct(Measure_Date_Time, format= "%Y-%m-%d %H:%M:%S"))

  combdata<-combdata %>% select(ID, Well_ID, timestamp, Absolute_Pressure_kPa, Degrees_C, site)
  return(combdata)
  }
