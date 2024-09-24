#' @title importWaterLevel: Imports and compiles views for wetland water level data package
#'
#' @description This function imports water level-related tables in the wetland RAM backend and
#' combines them into flattened views for the data package. Each view is added to a VIEWS_WL
#' environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr arrange filter left_join mutate rename right_join select
#' @importFrom purrr reduce
#' @importFrom lubridate month year
#'
#' @param type Select whether to use the default Data Source Named database (DSN) to import data or a
#' different database.
#' If "DSN" is selected, must specify name in odbc argument.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to "RAM_BE"}
#' \item{"file"}{A different database than default DSN}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "RAM_BE", which
#' is the back end of the MS Access RAM database.
#'
#' @param db_path Quoted path of database back end file, including the name of the backend.
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_WL environment. If \code{FALSE}, stores views in global environment
#'
#' @param export_data Logical. If TRUE, writes views to disk. If FALSE (Default), views are only
#' stored in specified R environment.
#'
#' @param export_path Quoted path to export views to. If blank, exports to working directory.
#'
#' @param export_all Logical. If TRUE, exports Accepted, Certified and Raw (winter) records. If FALSE (default),
#' only exports Accepted (pre-2021 QAQCed data) and Certified (post-2020 QAQCed data) records. Accepted and Certified
#' records are those that were collected during the growing season and that were QAQCed following procedures documented
#' in the protocol.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @examples
#' \dontrun{
#' # Import tables from database in specific folder:
#' importWaterLevel(type = 'file', db_path = './Data/NETN_RAM_Backend.mdb')
#'
#' # Export zip with Accepted or Certified records only
#' importWaterLevel(export_data = T, zip = T, export_all = F)
#' }
#'
#' @return Assigns RAM views to specified environment
#' @export

importWaterLevel <- function(export_protected = FALSE,
                            type = c('DSN', 'file'), odbc = 'RAM_BE',
                            db_path = NA, new_env = TRUE, export_data = FALSE,
                            export_all = FALSE, export_path = NA, zip = FALSE){

  #---- error handling ----
  stopifnot(class(export_protected) == 'logical')
  type <- match.arg(type)
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(export_data) == 'logical')
  stopifnot(class(export_all) == 'logical')
  stopifnot(class(zip) == 'logical')

  if(export_data == TRUE){
    if(is.na(export_path)){export_path <- getwd()
    } else if(!dir.exists(export_path)){stop("Specified export_path does not exist.")}

    # Normalize path for zip
    export_pathn <- normalizePath(export_path)

    # Add / to end of path if it wasn't specified.
    export_pathn <- if(!grepl("/$", export_pathn)){paste0(export_pathn, "\\")}
  }

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  # make sure db is on dsn list if type == DSN
  dsn_list <- odbc::odbcListDataSources()

  if(type == 'DSN' & !any(dsn_list$name %in% odbc)){
    stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}

  # check for db if type = file
  if(type == "file"){
    if(is.na(db_path)){stop("Must specify a path to the database for type = file option.")
    } else {
      if(file.exists(path) == FALSE){stop("Specified path or database does not exist.")}}
  }

  #---- import db tables ----
  tryCatch(
    db <- if (type == 'DSN'){
    db <- DBI::dbConnect(drv = odbc::odbc(), dsn = odbc)
    }
    else if (type == 'file'){
      db <- DBI::dbConnect(drv=odbc::odbc(),
                           .connection_string =
                           paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", path))
    },
      error = function(e){
      stop(error_mess)},
      warning = function(w){
      stop(error_mess)
    }
    )

  tbl_list1 <- DBI::dbListTables(db)[grepl("tbl|tlu|xref", DBI::dbListTables(db))]
  tbl_list <- tbl_list1[grepl("tbl_Well|tbl_Well_Visit|^tbl_Water_Level", tbl_list1)] # drops well tbls and queries

  pb = txtProgressBar(min = 0, max = length(tbl_list) + 2, style = 3)

  tbl_import <- lapply(seq_along(tbl_list),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- tbl_list[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  tbl_import <- setNames(tbl_import, tbl_list)

  if(new_env == TRUE){VIEWS_WL <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_WL} else {.GlobalEnv}

  list2env(tbl_import, envir = environment()) # all tables into fxn env
  setTxtProgressBar(pb, length(tbl_list) + 1)

  #---- Combine tables into views ----
  #--- tbl_wells
  tbl_wells1 <- tbl_Well |> rename(Well_ID = ID) |> arrange(Sample_Order) |>
    mutate(Launch_Year = year(Launch_Date))

  epa_codes <- data.frame(NWCA_Code = c("NWCA-R301", "NWCA-R302", "NWCA-R303", "NWCA-R304",
                                        "NWCA-R305", "NWCA-R306", "NWCA-R308", "NWCA-R309"),
                          Site_Code = c("DUCK", "WMTN", "BIGH", "GILM",
                                        "LIHU", "NEMI", "HEBR", "HODG"))
  tbl_wells2 <- left_join(tbl_wells1, epa_codes, by = "Site_Code")

  tbl_wells <- tbl_wells2[,c("Well_ID", "Well_Name", "Site_Code", "NWCA_Code",
                             "Easting", "Northing" ,
                            "UTM_Zone", "Launch_Date", "Launch_Year",
                            "Status", "Logger_Length", "MP_to_Bolt",
                            "Sample_Order", "Note", "Version")]

  #--- tbl_well_visits
  tbl_well_visits1 <- left_join(tbl_wells |> select(-Note),
                                tbl_Well_Visit, by = "Well_ID",
                                suffix = c("_well", "_well_visit")) |>
    mutate(Visit_Year = year(Visit_Date),
           Visit_Month = month(Visit_Date),
           Visit_Season = ifelse(Visit_Month %in% c(3, 4, 5, 6, 7), "Spring", "Fall")) |>
    select(-Status, -Version) |> rename(Visit_ID = ID, Visit_Time = Time)

  tbl_well_visits <- tbl_well_visits1[,
    c("Well_ID", "Well_Name", "Site_Code", "NWCA_Code",
      "Easting", "Northing", "UTM_Zone", "Sample_Order", "Logger_SN",
      "Visit_ID", "Visit_Date", "Visit_Year", "Visit_Month", "Visit_Season",
      "Logger_Length", "MP_to_Bolt", "Visit_Time", "Battery_Status",
      "Stick_Up_at_MP", "Post_1_Height", "Post_1_Distance", "Post_2_Height", "Post_2_Distance",
      "Water_Depth", "Water_Depth_Time", "Reset",
      "Note", "Certification_Level"
      )]

  #--- tbl_water_level
  tbl_wl1 <- right_join(tbl_wells, tbl_Water_Level, by = "Well_ID") |>
    mutate(Year = year(Measure_Date_Time),
           Month = month(Measure_Date_Time),
           doy = yday(Measure_Date_Time)) |>
    rename(WL_ID = ID, Note_WL = Note, Certification_Level = Flag,
           Note_Cert = Flag_Note) |> select(-Status, -Version)

  tbl_wl2 <- tbl_wl1[,
    c("Well_ID", "Well_Name", "Site_Code", "NWCA_Code", "Easting", "Northing", "UTM_Zone",
      "Logger_Length", "MP_to_Bolt", "Sample_Order", "WL_ID", "Measure_Date_Time", "Year", "Month", "doy",
      "Absolute_Pressure_kPa", "Degrees_C", "Note_WL", "Certification_Level", "Note_Cert")]

  # Fix certification levels (older years listed all records as A)
  tbl_wl2$Certification_Level[tbl_wl2$doy <= 134] <- "R"
  tbl_wl2$Certification_Level[tbl_wl2$doy >= 275] <- "R"

  #final tables to add to new env or global env: tbl_locations, tbl_visits
  tbl_water_level <-
    if(export_all == FALSE){filter(tbl_wl2, Certification_Level %in% c("A", "C"))
      } else {tbl_wl2}

  setTxtProgressBar(pb, length(tbl_list) + 2)
  close(pb)

  # final tables to add to new env or global env and print to disk
  final_tables <- list(tbl_wells, tbl_well_visits, tbl_water_level)

  final_tables <- setNames(final_tables,
                           c("tbl_wells", "tbl_well_visits", "tbl_water_level"))

  list2env(final_tables, envir = env)

  if(export_data == TRUE){
    # Export files
    if(zip == FALSE){
      invisible(lapply(seq_along(final_tables),
                       function(x){
                         dtbl = final_tables[[x]]
                         write.csv(dtbl, paste0(export_pathn, names(final_tables)[[x]], ".csv"),
                                   row.names = FALSE)
                       }))
    } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

      dir.create(tmp <- tempfile())

      invisible(lapply(seq_along(final_tables),
                       function(x){
                         dtbl = final_tables[[x]]
                         write.csv(dtbl,
                                   paste0(tmp, "\\", names(final_tables)[[x]], ".csv"),
                                   row.names = FALSE)}))

      file_list <- list.files(tmp)

      zip::zipr(zipfile = paste0(export_pathn, "NETN_Wetland_WL_Data_", format(Sys.Date(), "%Y%m%d"), ".zip"),
                root = tmp,
                files = file_list)
      # csvs will be deleted as soon as R session is closed b/c tempfile
    }
  }

  end_mess1 <- "Data package complete. Views are located in VIEWS_WL environment. "
  end_mess2 <- "Data package complete. Views are located in global environment. "

  if(export_data == FALSE){
    if(new_env == TRUE){print(end_mess1)
    } else if(new_env == FALSE){print(end_mess2)}
  } else if(export_data == TRUE){
    end_mess3 <- paste0("Files saved to: ", export_pathn, " ")
    end_mess4 <- paste0("Zip file saved to: ", export_pathn,
                        "NETN_Wetland_WL_Data_", format(Sys.Date(), "%Y%m%d"), ".zip ")

    if(new_env == TRUE & zip == TRUE){
      print(paste0(end_mess1, end_mess4))
    } else if(new_env == FALSE & zip == TRUE){
      print(paste0(end_mess2, end_mess4))
    } else if(new_env == TRUE & zip == FALSE){
      print(paste0(end_mess1, end_mess3))
    } else if(new_env == FALSE & zip == FALSE){
      print(paste0(end_mess2, end_mess3))
    }
  }
  } # End of function





