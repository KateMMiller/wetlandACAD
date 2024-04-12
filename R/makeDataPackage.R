#' @title makeDataPackage: Imports data from ACAD RAM backend and converts to data package format
#'
#' @description This function imports all tables in the wetland RAM backend and combines them into flattened views for the data package. Each view is added to a VIEWS_WETLAND environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
<<<<<<< HEAD
#' @importFrom dplyr all_of arrange collect filter full_join group_by left_join mutate rename summarize tbl
=======
#' @importFrom dplyr arrange collect group_by left_join mutate rename summarize tbl
>>>>>>> 68193c3b1307ee188c08a1f4fbe09f68e0cc1bd9
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
#'@param export_protected Logical. If TRUE, all records are exported. If FALSE (Default), only non-protected species are exported.
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


makeDataPackage <- function(data_type = c("RAM", "WELL", "all"), export_protected = FALSE,
                       type = c('DSN', 'file'), odbc = 'RAM_BE', path = NA, new_env = TRUE){

  data_type <- match.arg(data_type)
  stopifnot(class(protected) == 'logical')
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

  if(type == 'DSN' & !any(dsn_list$name %in% odbc)){
    stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}

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
  tbl_list <- switch(data_type,
                     "RAM" = tbl_list1[!grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)],
                     "WELL" = tbl_list1[grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)],
                     "all" = tbl_list1[grepl("^tbl|^xref|^tlu", tbl_list1)]) # drops queries
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
    tbl_Visit <- tbl_Visit |> mutate(Year = substr(Date, 1, 4))

    visit_tbl_list <- list(tbl_Visit,
                           tbl_Visit_Inundation |> rename(Flag_Inundation = Flag),
                           tbl_Visit_Saturation |> rename(Flag_Saturation = Flag),
                           tbl_Visit_Surface |> rename(Flag_Surface = Flag))
    visit_tbls <- reduce(visit_tbl_list, left_join, by = c("Visit_ID"))

    tbl_visits1 <- left_join(tbl_Location |> select(Code, Location_ID, Panel),
                              visit_tbls, by = "Location_ID") |>
                  arrange(Code, Date)

    tbl_visits <- left_join(tbl_visits1,
                            tbl_Protocol |> select(ID, Protocol_Version = Version),
                            by = c("Protocol_ID" = "ID"))

    first_cols <- c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type")
    last_cols <- c("Protocol_Version", "Checked", "Data_Verified_By", "Certification_Level")
    notes <- c("Notes_RAM01", "Notes_RAM02", "Notes_Topographic_Complexity", "Notes_Mosaic_Complexity",
               "Notes_RAM05", "Notes_RAM06", "Notes_RAM07", "Notes_RAM08", "Notes_RAM09",
               "Notes_RAM10", "Notes_RAM11", "Notes_Species", "Notes_H1")
    new_order <- c(first_cols,
                   names(tbl_visits[,!names(tbl_visits) %in% c(first_cols, notes, last_cols)]),
                   notes, last_cols)

    tbl_visits <- tbl_visits[, new_order]


    # tbl_plant_species
    tbl_species1 <- left_join(xref_Species_List |> rename(TSN = Plant_ID),
                              tlu_Plant |> select(Accepted_Latin_Name, TSN_Accepted, TSN, Latin_Name, Common,
                                                  Order, Family, Genus, PLANTS_Code, CoC_ME_ACAD, ACAD_ED,
                                                  Exotic, Invasive, Aquatic, Fern_Ally, Graminoid, Herbaceous,
                                                  Moss_Lichen, Shrub, Tree, Vine,  Author,
                                                  Canopy_Exclusion, Favorites, Protected_species),
                              by = c("TSN"))


    tbl_species2 <- left_join(tbl_visits |> select(all_of(first_cols)), tbl_species1, by = "Visit_ID")
    # Change -1 to 1
    binvars <- c("Quadrat_NE", "Quadrat_SE", "Quadrat_SW", "Quadrat_NW", "Coll")
    tbl_species2[,binvars][tbl_species2[,binvars] == -1] <- 1

    # Add qualifiers for plot visits that didn't run out tapes (RAM-17 all years; RAM-GM last year),
    # so percent freq isn't calculated as /4.
    tbl_species2$pct_freq <- 0

    tbl_species2 <- left_join(tbl_visits |> select(all_of(first_cols)), tbl_species1, by = "Visit_ID")
    # Change -1 to 1
    binvars <- c("Quadrat_NE", "Quadrat_SE", "Quadrat_SW", "Quadrat_NW", "Coll")
    tbl_species2[,binvars][tbl_species2[,binvars] == -1] <- 1

    # Add qualifier for site visits where we don't run out tapes and only one 15 min search around site is conducted.
    # After a check box is added to the database for this case, we can drop hard coding of it here.
    tbl_species2$limited_RAM <- ifelse(tbl_species2$Code %in% "R-17" |
                                      (tbl_species2$Code == "R-19" & tbl_species2$Year == 2023),
                                       1, 0)
    #table(tbl_species2$limited_RAM, tbl_species2$Code)
    #table(tbl_species2$limited_RAM, tbl_species2$Year)

    tbl_species2$quad_freq <- ifelse(tbl_species2$limited_RAM == 1,
                                     tbl_species2$Quadrat_NE * 100,
                                     ((tbl_species2$Quadrat_NE + tbl_species2$Quadrat_SE +
                                       tbl_species2$Quadrat_SW + tbl_species2$Quadrat_NW)/4)*100)


    first_cols <- c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type")
    last_cols <- c("Protocol_Version", "Checked", "Data_Verified_By", "Certification_Level")
    new_order <- c(first_cols,
                   "limited_RAM", "Latin_Name", "Common",
                   "Quadrat_NE", "Quadrat_SE", "Quadrat_SW", "Quadrat_NW", "quad_freq",
                   "Coll", "Comments", "TSN", "Order", "Family", "Genus",
                   "Exotic", "Invasive", "PLANTS_Code", "CoC_ME_ACAD",
                   "ACAD_ED", "Aquatic", "Fern_Ally", "Graminoid", "Herbaceous", "Moss_Lichen", "Shrub",
                   "Tree", "Vine", "Canopy_Exclusion",
                   "TSN_Accepted", "Accepted_Latin_Name", "Author", "Protected_species")

    tbl_species <- tbl_species2[,new_order]
    #setdiff(names(tbl_species2), names(tbl_species)) # check that dropped unwanted columns

    # tbl_buffer_stressors
    stress_tbls <- rbind(xref_Buffer_Stressor, xref_Hydro_Period_Stressor,
                         xref_Substrate_Stressor, xref_Vegetation_Stressor)


    tbl_stress1 <- left_join(stress_tbls, tlu_Stressor, by = "Stressor_ID")
    tbl_stress2 <- left_join(tbl_stress1, tlu_Stressor_Category, by = "Stressor_Category_ID")
    tbl_stress3 <- right_join(tbl_visits[,first_cols], tbl_stress2, by = "Visit_ID")

    tbl_stress_overall <- tbl_stress3 |> filter(Stressor %in% "Overall Ranking") |> select(-Stressor, Stressor_ID_Overall = Stressor_ID)
    tbl_stress_indiv <- tbl_stress3 |> filter(!Stressor %in% "Overall Ranking")
    head(tbl_stress_overall)
    head(tbl_stress_indiv)

    tbl_RAM_stress1 <- full_join(tbl_stress_overall, tbl_stress_indiv,
                                 by = c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                        "Location_Level", "Stressor_Category", "Stressor_Category_ID"),
                                 suffix = c("_Overall", "_Indiv")) |>
                       #filter(Severity_Indiv > 0) |>
                       select(all_of(first_cols), Location_Level, Stressor_Category, Stressor, Severity_Indiv, Severity_Overall,
                                     Stressor_Category_ID, Stressor_ID, Stressor_ID_Overall)

    miss_overall <- tbl_RAM_stress1 |> filter(Severity_Overall == 0 & Severity_Indiv > 0) |>
      select(Code, Year, Severity_Indiv, Severity_Overall, Stressor_ID, Stressor_ID_Overall, Visit_ID, Stressor_Category) |>
      arrange(Stressor_Category, Visit_ID)

    if(nrow(miss_overall) > 0){warning(paste0("The following Stressor_Overall records are missing a ranking where an individual stressor was recorded.",
                                              "\n",
                                              paste0(miss_overall[, c("Code", "Year", "Visit_ID", "Stressor_Category", "Stressor_ID", "Stressor_ID_Overall",
                                                                     "Severity_Indiv", "Severity_Overall")], collapse = "\n ")))}

    miss_indiv <- tbl_RAM_stress1 |> group_by(Code, Year, Stressor_ID, Stressor_Category) |>
      summarize(num_indiv_stress = sum(Severity_Indiv > 0),
                stress_overall = sum(Severity_Overall > 0), .groups = 'drop') |>
      filter(num_indiv_stress == 0 & stress_overall > 0)

    #++++++++++++++ ENDED HERE ++++++++++++++++++++
    # Lots of returns- need to look into why (may need to add another grouping variable)

    head(miss_indiv)
    # filter(Severity_Overall > 0 & Severity_Indiv == 0) |>
      # select(Code, Year, Severity_Indiv, Severity_Overall, Stressor_ID, Stressor_ID_Overall, Visit_ID, Stressor_Category) |>
      # arrange(Stressor_Category, Visit_ID)

    write.csv(fixes, "missing_Overalls.csv", row.names = F)

    head(tbl_RAM_stress1)

    head(xref_Buffer_Stressor)
    head(xref_Hydro_Period_Stressor)
    head(xref_Substrate_Stressor)
    head(xref_Vegetation_Stressor)
    head(tlu_Stressor)

    head(xref_Visit_Hydrologic_Stressor)
    head(xref_Visit_Water)

    head(tlu_Stressor)

    head(xref_Buffer_Width)

  }
    if(data_type %in% c("WELL", "all")){


  }

  if(export_protected == TRUE){ #Keep all records
  } else {#Use tlu_Species$Protected_species column to filter protected species out of tbl_species and
    #RAM data with species attached.
    }

  if(export_protected == TRUE){ #Keep all records
  } else {#Use tlu_Species$Protected_species column to filter protected species out of tbl_species and
    #RAM data with species attached.
    }

  close(pb)

  #final tables to add to new env or global env: tbl_locations, tbl_visits

  print(ifelse(new_env == TRUE,
        paste0("Import complete. Views are located in VIEWS_WETLAND environment."),
        paste0("Import complete. Views are located in global environment.")
        ))

  } # End of function





