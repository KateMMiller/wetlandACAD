#' @title makeDataPackage: Imports data from ACAD RAM backend and converts to data package format
#'
#' @description This function imports all tables in the wetland RAM backend and combines them into flattened views for the data package. Each view is added to a VIEWS_WETLAND environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr arrange collect everything group_by left_join rename summarize tbl
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#'
#' @param data_type
#' \describe{
#' \item{"RAM"}{Default. Only imports the data collected in rapid assessment sites.}
#' \item{"WELL"}{Only imports water level data collected in sentinel site wells.}
#' \item{"all"}{Imports all data tables. Note importing all files can be slow.}
#' }
#'
#' @param type Select whether to use the default Data Source Named database (DSN) to import data or a different database. If "DSN" is selected, must specify name in odbc argument.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to "RAM_BE"}
#' \item{"file"}{A different database than default DSN}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "RAM_BE", which is the front end of the MS Access RAM database that contains the queries to import.
#'
#' @param path Quoted path of database backend file, including the name of the backend.
#' @return Assigns database tables to global environment
#'
#' @examples
#' \dontrun{
#' # Import tables from database in specific folder:
#' makeDataPackage(type = 'file', path = './Data/NETN_RAM_Backend.mdb')
#'
#' # Import ODBC named database
#' makeDataPackage(type = 'DSN', odbc = "RAM_BE")
#' }
#'
#' @export


makeDataPackage <- function(data_type = c("RAM", "WELL", "all"),
                       type = c('DSN', 'file'), odbc = 'RAM_BE', path = NA, new_env = TRUE){

  data_type <- match.arg(data_type)
  type <- match.arg(type)
  stopifnot(class(new_env) == 'logical')

  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }
  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("dbplyr", quietly = TRUE)){
    stop("Package 'dbplyr' needed for this function to work. Please install it.", call. = FALSE)
  }

  dsn_list <- odbc::odbcListDataSources()

  if(type == 'DSN' & !any(dsn_list$name %in% DSN)){
    stop(paste0("Specified DSN ", DSN, " is not a named database source." ))}

  if(type == "file"){
    if(is.na(path)){stop("Must specify a path to the database for type = file option.")
    } else {
      if(file.exists(path) == FALSE){stop("Specified path or database does not exist.")}}
  }

  # if(new_env == TRUE){
  #   VIEWS_WETLAND <<- new.env()
  #   env = VIEWS_WETLAND
  # } else { env = .GlobalEnv }

tryCatch(
  db <- if (type == 'DSN'){
    db <- DBI::dbConnect(drv = odbc::odbc(), dsn = DSN)
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
  tbl_list <- switch(data_type,
                     "RAM" = tbl_list1[!grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)],
                     "WELL" = tbl_list1[grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)],
                     "all" = tbl_list1)
  #tail(tbl_list)

  pb = txtProgressBar(min = 0, max = length(tbl_list), style = 3)

  # Import tables using their names and show progress bar
  tbl_import <- lapply(seq_along(tbl_list),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- tbl_list[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  tbl_import <- setNames(tbl_import, tbl_list)

  # if(new_env == TRUE){
  #   VIEWS_WETLAND <<- new.env()
  #   list2env(tbl_import, envir = VIEWS_WETLAND)
  # } else {
    list2env(tbl_import, envir = .GlobalEnv)#}

  # Combine tables into views by data types
  if(data_type %in% c("RAM", "all")){
    # Create tbl_locations by reshaping xrefs to have 1 record per location
    xref_Loc_Diff1 <- left_join(xref_Location_Difficulty, tlu_Difficulty, by = "Difficulty_ID")
    xref_Loc_Diff <- xref_Loc_Diff1 |> group_by(Location_ID) |>
      summarize(Access_Difficulty = paste0(Difficulty_Full, collapse = "; "))

    xref_Loc_Req1 <- left_join(xref_Location_Requirement, tlu_Requirement, by = c("Requirement_ID" = "ID"))
    xref_Loc_Req <- xref_Loc_Req1 |> group_by(Location_ID) |>
      summarize(Access_Requirement = paste0(Location_Requirement_Comments, collapse = "; "))

    xref_Loc_Hydro1 <- left_join(xref_Location_Hydrology, tlu_Hydrology, by = "Wetland_Hydrology_ID")
    xref_Loc_Hydro1$Wetland_Hydro <- gsub(" ", "_", substr(xref_Loc_Hydro1$Wetland_Hydrology,
                                            6, nchar(xref_Loc_Hydro1$Wetland_Hydrology)))
    xref_Loc_Hydro1$present <- 1
    xref_Loc_Hydro_wide <- pivot_wider(xref_Loc_Hydro1 |>
                                         select(Location_ID, Wetland_Hydro,
                                                Wetland_Hydrology_Comments, present),
                                       names_from = Wetland_Hydro,
                                       values_from = present, values_fill = 0) |>
      group_by(Location_ID) |> summarize(Wetland_Hydro_Comments =
                                           paste0(Wetland_Hydrology_Comments, collapse = ": "))

    loc_tbl_list <- list(tbl_Location, xref_Loc_Diff, xref_Loc_Req, xref_Loc_Hydro_wide)
    tbl_locations <- reduce(loc_tbl_list, left_join, by = "Location_ID") |> arrange(Code)

    # tbl_visits
    tbl_Visit <- tbl_Visit |> mutate(Year = substr(Date, 1, 4)) |>
      select(Location_ID, Visit_ID, Date, Year, Visit_Type, everything())

    visit_tbl_list <- list(tbl_Visit,
                           tbl_Visit_Inundation |> rename(Flag_Inundation = Flag),
                           tbl_Visit_Saturation |> rename(Flag_Saturation = Flag),
                           tbl_Visit_Surface |> rename(Flag_Surface = Flag))
    visit_tbls <- reduce(visit_tbl_list, left_join, by = c("Visit_ID"))

    tbl_visits1 <- left_join(tbl_Location |> select(Location_ID, Code),
                              visit_tbls, by = "Location_ID") |>
                  arrange(Code, Date)
    tbl_visits <- left_join(tbl_visits1,
                            tbl_Protocol |> select(ID, Protocol_Version = Version),
                            by = c("Protocol_ID" = "ID"))

    # tbl_plant_species
    tbl_species1 <- left_join(xref_Species_List,
                              tlu_Plant |> select(Accepted_Latin_Name, TSN_Accepted, TSN, Latin_Name, Common,
                                                  Order, Family, Genus, PLANTS_Code, CoC_ME_ACAD, ACAD_ED,
                                                  Exotic, Invasive, Aquatic, Fern_Ally, Graminoid, Herbaceous,
                                                  Moss_Lichen, Shrub, Tree, Vine,  Author,
                                                  Canopy_Exclusion, Favorites, Protected_species),
                              by = c("Plant_ID" = "TSN"))
    head(tbl_species1)
    head(xref_Species_List)

    }

  if(data_type %in% c("WELL", "all")){

  }

  close(pb)

  #final tables to add to new env or global env: tbl_locations, tbl_visits

  print(ifelse(new_env == TRUE,
        paste0("Import complete. Views are located in VIEWS_WETLAND environment."),
        paste0("Import complete. Views are located in global environment.")
        ))

  } # End of function





