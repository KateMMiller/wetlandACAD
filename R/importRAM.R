#' @title importRAM: Imports and compiles views for wetland RAM data package
#'
#' @description This function imports RAM-related tables in the wetland RAM backend and combines them
#' into flattened views for the data package. Each view is added to a VIEWS_RAM environment in your
#' workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr all_of arrange collect filter full_join group_by left_join mutate rename right_join summarize tbl
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#'
#' @param export_protected Logical. If TRUE, all records are exported. If FALSE (Default), only non-protected
#'species are exported.
#'
#' @param type Select whether to use the default Data Source Named database (DSN) to import data, a
#' different database, or a data package with CSVs. If "DSN" is selected, must specify name in odbc argument.
#' If dbfile, csv, or zip are selected, must specify a filepath.
#' \describe{
#' \item{"DSN"}{Default. DSN database. If odbc argument is not specified, will default to "RAM_BE"}
#' \item{"dbfile"}{A different database than default DSN}
#' \item{"csv"}{Import csv views that have already been compiled as data package.}
#' \item{"zip"}{Import zip file containing csvs from a data package.}
#' }
#'
#' @param odbc DSN of the database when using type = DSN. If not specified will default to "RAM_BE", which
#' is the back end of the MS Access RAM database.
#'
#' @param filepath Quoted path of database back end dbfile, csvs or zip. If dbfile or zip are the type, must
#' include the name of the database or zip file (see examples).
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_RAM environment. If \code{FALSE}, stores views in global environment
#'
#' @param export_data Logical. If TRUE, writes views to disk. If FALSE (Default), views are only
#' stored in specified R environment.
#'
#' @param export_path Quoted path to export views to. If blank, exports to working directory.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @examples
#' \dontrun{
#'
#' filepath <- "C:/NETN/R_Dev/data/wetland_data_package/"
#' ex_path <- filepath
#'
#' # Import tables from database in specific folder and only export non-protected:
#' importRAM(type = 'dbfile', filepath = paste0(filepath, "NETN_RAM_20241030.accdb"), export_path = ex_path, export_data = T)
#'
#' # Import ODBC named database into global env with protected species and export as zip file
#' importRAM(type = 'DSN', odbc = "RAM_BE", new_env = F, export_protected = F, export_data = T, zip = T, export_path = ex_path)
#'
#' # Import csvs of data package without protected species
#' importRAM(type = 'csv', filepath = filepath, export_data = T, export_path = ex_path, zip = T)
#'
#' # Import csvs of data package with protected species
#' importRAM(type = 'csv', filepath = filepath, export_data = T, export_path = ex_path, zip = T, export_protected = T)
#'
#' # Import zip of data package containing csvs but don't export anything
#' importRAM(type = 'zip', filepath = paste0(filepath, "NETN_Wetland_RAM_Data_20241209_NPSonly.zip"))
#'
#' # Import zip of data package containing csvs without protected species but don't export anything
#' importRAM(type = 'zip', filepath = paste0(filepath, "NETN_Wetland_RAM_Data_20241209_public.zip"))
#'
#' }
#'
#' @return Compiles and assigns RAM views to specified environment
#' @export

importRAM <- function(export_protected = FALSE,
                      type = c('DSN'), odbc = 'RAM_BE',
                      filepath = NA, new_env = TRUE, export_data = FALSE,
                      export_path = NA, zip = FALSE){

  #---- error handling ----
  stopifnot(class(export_protected) == 'logical')
  type <- match.arg(type, c("DSN", "dbfile", "csv", "zip"))
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(export_data) == 'logical')
  stopifnot(class(zip) == 'logical')

  if(!requireNamespace("sf", quietly = T)){
    stop("Package 'sf' needed to generate lat/long coordinates. Please install it.", call. = FALSE)}

  if(export_data == TRUE){
    if(is.na(export_path)){export_path <- getwd()
    } else if(!dir.exists(export_path)){stop("Specified export_path does not exist.")}

    # Normalize path for zip
    export_pathn <- normalizePath(export_path)

    # Add / to end of path if it wasn't specified.
    export_pathn <- if(!grepl("/$", export_pathn)){paste0(export_pathn, "\\")}
  }

  # check that filepath was specified for non-DSN options
  if(type %in% c('dbfile', 'csv', 'zip')){
    if(is.na(filepath)){stop(paste0("Must specify a filepath to the database when type = 'dbfile', 'csv', or 'zip'."))
    } else if(!file.exists(filepath)){
      stop(paste0("Specified file path does not exist. ",
                  ifelse(grepl("sharepoint", filepath), " Note that file paths from Sharepoint or Teams are not accessible.",
                         "")))}}

  if(type %in% "csv" & !grepl("/$", filepath)){filepath <- paste0(filepath, "/")} # add / to end of filepath if doesn't exist

  # Check if type = 'csv' was specified, but .zip file is filepath
  if(type == 'csv' & grepl(".zip", filepath)){
    stop("Specified a zip file in filepath. Must use type = 'zip' instead of 'csv'.")}


  if(!requireNamespace("odbc", quietly = TRUE)){
    stop("Package 'odbc' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("DBI", quietly = TRUE)){
    stop("Package 'DBI' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("zip", quietly = TRUE) & zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  if(type %in% c("DSN", "dbfile")){
  # make sure db is on dsn list if type == DSN
  dsn_list <- odbc::odbcListDataSources()

  if(type == 'DSN' & !any(dsn_list$name %in% odbc)){
    stop(paste0("Specified DSN ", odbc, " is not a named database source." ))}

  # check for db if type = file
  if(type == "dbfile"){
    if(is.na(filepath)){stop("Must specify a path to the database for type = file option.")
    } else {
      if(file.exists(paste0(filepath)) == FALSE){stop("Specified path to database does not exist.")}}
  }
  }

  # Set up objected used by all file types
  views <- list("locations", "visits", "visit_history", "RAM_stressors",
                "AA_char", "species_list", "species_by_strata", "vertical_complexity",
                "tlu_Plant")


  if(new_env == TRUE){VIEWS_RAM <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_RAM} else {.GlobalEnv}

  #---- import db tables ----
  if(type %in% c("DSN", "dbfile")){
  tryCatch(
    db <- if (type == 'DSN'){
    db <- DBI::dbConnect(drv = odbc::odbc(), dsn = odbc)
    }
    else if (type == 'dbfile'){
      db <- DBI::dbConnect(drv=odbc::odbc(),
                           .connection_string =
                           paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=", filepath))
    },
      error = function(e){
      stop(e)},
      warning = function(w){
      stop(w)
    }
    )

  tbl_list1 <- DBI::dbListTables(db)[grepl("tbl|tlu|xref", DBI::dbListTables(db))]
  tbl_list <- tbl_list1[!grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)] # drops well tbls and queries

  pb = txtProgressBar(min = 0, max = length(tbl_list) + 3, style = 3)

  tbl_import <- lapply(seq_along(tbl_list),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- tbl_list[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  tbl_import <- setNames(tbl_import, tbl_list)

  list2env(tbl_import, envir = environment()) # all tables into fxn env

  #---- Combine tables into views ----
    #--- tbl_locations
    # Create tbl_locations by reshaping xrefs to have 1 record per location

    xref_Loc_Diff1 <- left_join(xref_Location_Difficulty, tlu_Difficulty, by = "Difficulty_ID")
    xref_Loc_Diff1$Difficulty_Full[xref_Loc_Diff1$Difficulty_Full == "Other"] <- NA_character_

    xref_Loc_Diff <- xref_Loc_Diff1 |> group_by(Location_ID) |>
      summarize(Access_Difficulty1 = paste0(Difficulty_Full[!is.na(Difficulty_Full)], collapse = "; "),
                Access_Difficulty2 = paste0(
                  Location_Difficulty_Comments[!is.na(Location_Difficulty_Comments)],
                  collapse = "; "),
                Access_Difficulty = paste(Access_Difficulty1, Access_Difficulty2,
                                          collapse = "; ", sep = "; ")) |>
      select(-Access_Difficulty1, -Access_Difficulty2) |> as.data.frame()

    xref_Loc_Diff$Access_Difficulty <- sub("^; +", "", xref_Loc_Diff$Access_Difficulty) # clean up string

    xref_Loc_Req1 <- left_join(xref_Location_Requirement, tlu_Requirement, by = c("Requirement_ID" = "ID"))
    xref_Loc_Req <- xref_Loc_Req1 |> group_by(Location_ID) |>
      summarize(Access_Requirement = paste0(Location_Requirement_Comments, collapse = "; "))

    xref_Loc_Hydro1 <- left_join(xref_Location_Hydrology, tlu_Hydrology, by = "Wetland_Hydrology_ID")
    xref_Loc_Hydro1$Wetland_Hydro <- gsub(" ", "_", substr(xref_Loc_Hydro1$Wetland_Hydrology,
                                            6, nchar(xref_Loc_Hydro1$Wetland_Hydrology)))

    # Handling adding present column if df is empty
    xref_Loc_Hydro2 <- xref_Loc_Hydro1
    if(nrow(xref_Loc_Hydro2) > 0){
      xref_Loc_Hydro2$present <- 1
    } else {
      xref_Loc_Hydro2[1,] <- NA
      xref_Loc_Hydro2$present <- 1
    }

    xref_Loc_Hydro_wide <- pivot_wider(xref_Loc_Hydro2 |>
                                         select(Location_ID, Wetland_Hydro,
                                                Wetland_Hydrology_Comments, present),
                                       names_from = Wetland_Hydro,
                                       values_from = present, values_fill = 0) |>
      group_by(Location_ID) |> mutate(Wetland_Hydro_Comments =
                                           paste0(Wetland_Hydrology_Comments, collapse = ": "))

    if(any(names(xref_Loc_Hydro_wide) %in% c("NA"))){
      xref_Loc_Hydro_wide <- xref_Loc_Hydro_wide[ ,-which(names(xref_Loc_Hydro_wide) %in% "NA")]}

    # Handling missing hydro options that then don't show up as columns after pivot
    hydro_names <- c("Location_ID", "Wetland_Hydrology_Comments", "Saturated_Soils", "Standing_Water",
                     "Shallow_Roots", "Water_Marks", "Water_Carried_Debris", "Bare_Areas", "Buttressed_Trunks",
                     "Water_Stained_Leaves", "Oxidized_Rhizospheres", "Other", "Floating_Mat", "Wetland_Hydro_Comments")

    missing_hydro <- setdiff(hydro_names, names(xref_Loc_Hydro_wide))
    xref_Loc_Hydro_wide[missing_hydro] <- NA
    if(nrow(xref_Loc_Hydro1) == 0){xref_Loc_Hydro_wide <- xref_Loc_Hydro_wide[0,]}

    loc_tbl_list <- list(tbl_Location, xref_Loc_Diff, xref_Loc_Req, xref_Loc_Hydro_wide)
    tbl_locations1 <- reduce(loc_tbl_list, left_join, by = "Location_ID") |> arrange(Code)

    tbl_locations2 <- left_join(tbl_locations1,
                                tlu_Predominant_Category |> rename(FWS_Class_Code = Code),
                                by = "Predominant_Category_ID")

    tbl_locations3 <- left_join(tbl_locations2,
                                tlu_Class |> rename(HGM_Class = Class),
                                by = "Class_ID")

    tbl_locations4 <- left_join(tbl_locations3,
                                tlu_Sub_Class |> rename(HGM_Sub_Class = Sub_Class),
                                by = c("Class_ID", "Sub_Class_ID"))

    tbl_locations5 <- left_join(tbl_locations4 |> rename(AA_Area = Area),
                                tlu_AA_Layout |> rename(AA_Layout = Shape),
                                by = "Layout_ID") |>
      mutate(X = Easting, Y = Northing)

    latlon <- sf::st_as_sf(tbl_locations5, coords = c("X", "Y"), crs = 26919) |>
      sf::st_transform(crs = 4269) #NAD83; WGS84 is 4326

    latlon_df <- latlon |>
      mutate(Code = Code,
             Location_ID = Location_ID,
             Latitude = sf::st_coordinates(latlon)[,2],
             Longitude = sf::st_coordinates(latlon)[,1]) |>
      data.frame() |> select(Code, Location_ID, Latitude, Longitude)

    tbl_locations6 <- left_join(tbl_locations5, latlon_df, by = c("Code", "Location_ID"))

    tbl_locations <- tbl_locations6[,c("Code", "Location_ID", "Panel", "Date_Established",
                                       "Contact_ID", "Easting", "Northing",
                                       "Latitude", "Longitude",
                                       "UTM_Zone", "Description", "FWS_Class_Code",
                                       "HGM_Class", "HGM_Sub_Class", "AA_Layout", "AA_Area",
                                       "Directions",
                                       "Location_Comments", "Access_Comments",
                                       "Notes_AA2", "Access_Difficulty", "Access_Requirement",
                                       "Saturated_Soils", "Standing_Water", "Shallow_Roots",
                                       "Water_Marks", "Water_Carried_Debris", "Bare_Areas",
                                       "Floating_Mat", "Buttressed_Trunks",
                                       "Water_Stained_Leaves", "Oxidized_Rhizospheres", "Other",
                                       "Wetland_Hydro_Comments")]

    tbl_locations <- arrange(tbl_locations, Code)
    names(tbl_locations)[names(tbl_locations) == "Easting"] <- "xCoordinate"
    names(tbl_locations)[names(tbl_locations) == "Northing"] <- "yCoordinate"

    setTxtProgressBar(pb, length(tbl_list) + 1)

    #--- tbl_visits
    tbl_Visit <- tbl_Visit |> mutate(Year = substr(Date, 1, 4)) |>
      mutate(limited_RAM = ifelse(AA_Point == "Yes", 1, 0))

    visit_tbl_list <- list(tbl_Visit,
                           tbl_Visit_Inundation |> rename(Flag_Inundation = Flag),
                           tbl_Visit_Saturation |> rename(Flag_Saturation = Flag),
                           tbl_Visit_Surface |> rename(Flag_Surface = Flag))
    visit_tbls <- reduce(visit_tbl_list, left_join, by = c("Visit_ID"))

    tbl_visits1 <- left_join(tbl_Location |> select(Code, Location_ID, Panel),
                              visit_tbls, by = "Location_ID") |>
                  arrange(Code, Date)

    tbl_visits2 <- left_join(tbl_visits1,
                             tbl_Protocol |> select(ID, Protocol_Version = Version),
                             by = c("Protocol_ID" = "ID"))

    tbl_visits3 <- left_join(tbl_visits2, tlu_Invasive_Coverage, by = "Invasive_Coverage_ID") |>
      rename(Invasive_Cover_Class = Invasive_Coverage)

    # Prepare and add buffer widths
    tbl_buffs1 <- left_join(xref_Buffer_Width, tlu_Buffer_Direction, by = "Buffer_Direction_ID") |>
      select(-Buffer_Direction_ID) |>
      pivot_wider(names_from = Buffer_Direction, values_from = Width_m,
                  names_prefix = "Buffer_Width_") |>
      mutate(Buffer_Width_Avg = (Buffer_Width_N + Buffer_Width_NE + Buffer_Width_E +
                                 Buffer_Width_SE + Buffer_Width_S + Buffer_Width_SW +
                                 Buffer_Width_W + Buffer_Width_NW)/8)

    tbl_visits4 <- left_join(tbl_visits3, tbl_buffs1, by = "Visit_ID")
    tbl_visits5 <- left_join(tbl_visits4, tlu_Perimeter, by = "Perimeter_ID") |>
      rename(Buffer_Perim_Percent = Percent)

    # add top 3 water sources
    tbl_water1 <- left_join(xref_Visit_Water, tlu_Water, by = "Water_ID") |>
      select(-Water_ID, -Present) |> filter(Rank > 0)

    tbl_water2 <-tbl_water1 |>
      group_by(Visit_ID) |>
      mutate(Flag_Water_Source =
               paste(Flag[!is.na(Flag)], collapse = "; ")) |>
      ungroup() |>
      select(-Flag) |> arrange(Visit_ID, Rank) |>
      pivot_wider(names_from = Rank, values_from = Source, names_prefix = "Water_Source_")

    tbl_visits6 <- left_join(tbl_visits5, tbl_water2, by = "Visit_ID")

    first_cols <- c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type", "limited_RAM")
    mid_cols <- c("Weather",  "Prior_Weather", "Ditch_Present", "Depth_1", "Depth_2", "Depth_3",
                  "Flag_Water_Source", "Water_Source_1", "Water_Source_2", "Water_Source_3",
                  "Mosaic_Complexity", "SphagnumMoss", "Sphagnum_Cover", "Invasive_Cover_Class",
                  "Invasive_Cover", "Buffer_Width_N", "Buffer_Width_NE", "Buffer_Width_E",
                  "Buffer_Width_SE", "Buffer_Width_S", "Buffer_Width_SW", "Buffer_Width_W",
                  "Buffer_Width_NW", "Buffer_Width_Avg", "Buffer_Perim_Percent", "AlgalMatOrCrust",
                  "AquaticInvertebrate", "BioticCrust", "DrainagePatterns", "DriftDeposits",
                  "Flag_Inundation", "IronDeposits", "MarlDeposits", "MossTrimLines", "SaltCrust",
                  "SedimentDeposits", "SparselyVegetatedConcaveSurfaces", "SurfaceSoilCracks", "WaterMarks",
                  "WaterStainedLeaves", "TrueAquaticPlants", "CrayfishBurrows", "DrySeasonWaterTable",
                  "FiddlerCrabBurrows", "Flag_Saturation", "HydrogenSulfideOdor", "SaltDeposits",
                  "SurficialThinMuck", "OxidizedRhizospheres", "Flag_Surface", "GeomorphicPosition",
                  "HighWaterTable", "MicrotopographicRelief", "ShallowAquitard", "SoilSaturation",
                  "StuntedOrStressedPlants", "SurfaceWater")
    last_cols <- c("Protocol_Version", "Checked", "Data_Verified_By", "Certification_Level")
    notes <- c("Notes_AA_Point", "Notes_RAM01", "Notes_RAM02",
               "Notes_Topographic_Complexity", "Notes_Mosaic_Complexity",
               "Notes_RAM05", "Notes_RAM06", "Notes_RAM07", "Notes_RAM08", "Notes_RAM09",
               "Notes_RAM10", "Notes_RAM11", "Notes_Species", "Notes_H1")
    new_order <- c(first_cols,
                   mid_cols,
                   #names(tbl_visits5[,!names(tbl_visits5) %in% c(first_cols, mid_cols, notes, last_cols)]),
                   notes, last_cols)

    tbl_visits <- tbl_visits6[, new_order]
    names(tbl_visits)[names(tbl_visits) == "Sphagnum_Cover"] <- "Bryophyte_Cover"
    tbl_visits$Year <- as.integer(tbl_visits$Year)

    #settbl_visits#setdiff(names(tbl_visits6), names(tbl_visits)) # dropped unwanted names

    #--- tbl_visit_history
    tbl_visit_history <- right_join(tbl_visits[,first_cols], tbl_Visit_Metadata, by = c("Location_ID", "Visit_ID")) |>
      arrange(Code, Year, Updated_Table)

    #--- tbl_AA_char
    # Topo Complexity and Hydro sources
    tbl_topo1 <- left_join(xref_Topo_Complexity, tlu_Topo_Complexity, by = "Topography_ID")
    tbl_topo2 <- right_join(tbl_visits[,first_cols], tbl_topo1, by = "Visit_ID") |>
      filter(Observed == -1) |>
      mutate(Type = "Topographic_Complexity",
             Present = ifelse(Observed == -1, 1, 0),
             Flag = NA_character_) |>
      rename(Feature = Topography) |>
      select(-Topography_ID, -Observed)

    # Only take sources that are present. Top 3 sources (rank) is in tbl_visits
    tbl_water1 <- left_join(xref_Visit_Water, tlu_Water, by = "Water_ID") |>
      mutate(Present = ifelse(Present == -1, 1, 0)) |> select(-Water_ID, -Rank)
    tbl_water2 <- right_join(tbl_visits[,first_cols], tbl_water1, by = "Visit_ID") |>
      mutate(Type = "Water_Sources") |>
      rename(Feature = Source)

    tbl_AA_char <- rbind(tbl_topo2, tbl_water2) |> filter(Present == 1) |>
      arrange(Code, Year, Type, Feature) |> select(-Present)

    #--- tbl_species_list
    tbl_species1 <- left_join(xref_Species_List |> rename(TSN = Plant_ID),
                              tlu_Plant |> select(Accepted_Latin_Name, TSN_Accepted, TSN, Latin_Name, Common,
                                                  Order, Family, Genus, PLANTS_Code, CoC_ME_ACAD, ACAD_ED,
                                                  Exotic, Invasive, Aquatic, Fern_Ally, Graminoid, Herbaceous,
                                                  Moss_Lichen, Shrub, Tree, Vine, Synonym, Author,
                                                  Canopy_Exclusion, Favorites, Protected_species),
                              by = c("TSN"))
    # Change -1 to 1
    binvars <- c("Quadrat_NE", "Quadrat_SE", "Quadrat_SW", "Quadrat_NW", "Coll")
    tbl_species1[,binvars][tbl_species1[,binvars] == -1] <- 1

    tbl_species2 <- left_join(tbl_visits |> select(all_of(first_cols)), tbl_species1, by = "Visit_ID") |>
      mutate(quad_freq = ifelse(limited_RAM == 1, Quadrat_NE * 100,
                                ((Quadrat_NE + Quadrat_SE +
                                    Quadrat_SW + Quadrat_NW)/4)*100))

    first_cols <- c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type", "limited_RAM")
    last_cols <- c("Protocol_Version", "Checked", "Data_Verified_By", "Certification_Level")
    new_order <- c(first_cols,
                   "Latin_Name", "Common",
                   "Quadrat_NE", "Quadrat_SE", "Quadrat_SW", "Quadrat_NW", "quad_freq",
                   "Coll", "Comments", "TSN", "Order", "Family", "Genus",
                   "Exotic", "Invasive", "PLANTS_Code", "CoC_ME_ACAD",
                   "ACAD_ED", "Aquatic", "Fern_Ally", "Graminoid", "Herbaceous", "Moss_Lichen", "Shrub",
                   "Tree", "Vine", "Canopy_Exclusion",
                   "TSN_Accepted", "Accepted_Latin_Name", "Synonym", "Author", "Protected_species")

    tbl_species_list <- tbl_species2[,new_order]
    #setdiff(names(tbl_species2), names(tbl_species)) # check that dropped unwanted columns
    names(tbl_species_list)[names(tbl_species_list) == "Coll"] <- "Collected"

    setTxtProgressBar(pb, length(tbl_list) + 2)

    #--- tbl_vertical_complexity
    tbl_vert1 <- left_join(xref_Vert_Complexity, tlu_Vert_Complexity, by = "Vert_Complexity_ID")
    tbl_vert2 <- left_join(tbl_vert1, tlu_Strata, by = "Strata_ID") |> rename(Cover_Class = Vert_Complexity)
    tbl_vert2$Cover_Class[tbl_vert2$Vert_Complexity_ID == 6] <- "0%"

    tbl_vertical_complexity <- right_join(tbl_visits[,first_cols], tbl_vert2, by = "Visit_ID")

    #--- tbl_species_by_strata
    tbl_pcomp1 <- left_join(xref_Plant_Complexity, tlu_Strata, by = "Strata_ID")
    tbl_pcomp2 <- left_join(tbl_pcomp1, tlu_Plant, by = "TSN")
    tbl_pcomp3 <- right_join(tbl_visits[,first_cols], tbl_pcomp2, by = "Visit_ID")

    tbl_species_by_strata <-
      tbl_pcomp3[,c(first_cols, "Strata", "Strata_ID", "Latin_Name", "Common", "Percent_Cover",
                    "TSN", "Order", "Family", "Genus", "Exotic", "Invasive", "PLANTS_Code",
                    "CoC_ME_ACAD", "ACAD_ED", "Aquatic", "Fern_Ally", "Graminoid", "Herbaceous",
                    "Moss_Lichen", "Shrub", "Tree", "Vine", "Canopy_Exclusion", "TSN_Accepted",
                    "Accepted_Latin_Name", "Synonym", "Author", "Protected_species")]

    #setdiff(names(tbl_pcomp3), names(tbl_species_by_strata))

    #--- tbl_RAM_stressors
    stress_tbls <- rbind(xref_Buffer_Stressor, xref_Hydro_Period_Stressor,
                         xref_Substrate_Stressor, xref_Vegetation_Stressor)

    tbl_stress1 <- left_join(stress_tbls, tlu_Stressor, by = "Stressor_ID")
    tbl_stress2 <- left_join(tbl_stress1, tlu_Stressor_Category, by = "Stressor_Category_ID")
    tbl_stress3 <- right_join(tbl_visits[,first_cols], tbl_stress2, by = "Visit_ID")

    tbl_stress_overall <- tbl_stress3 |> filter(Stressor %in% "Overall Ranking") |>
      select(-Stressor, Stressor_ID_Overall = Stressor_ID)
    tbl_stress_indiv <- tbl_stress3 |> filter(!Stressor %in% "Overall Ranking")

    tbl_RAM_stress1 <- full_join(tbl_stress_overall, tbl_stress_indiv,
                                 by = c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                        "limited_RAM",
                                        "Location_Level", "Stressor_Category", "Stressor_Category_ID"),
                                 suffix = c("_Overall", "_Indiv")) |>
                       #filter(Severity_Indiv > 0) |>
                       select(all_of(first_cols), Location_Level, Stressor_Category,
                              Stressor, Severity_Indiv, Severity_Overall) |>
                       mutate(Flag = NA_character_) # for xref_visit_hydro join

    miss_overall <- tbl_RAM_stress1 |> filter(Severity_Overall == 0 & Severity_Indiv > 0) |>
      select(Code, Year, Severity_Indiv, Severity_Overall, Visit_ID, Stressor_Category) |>
      arrange(Stressor_Category, Visit_ID)

    if(nrow(miss_overall) > 0){
      warning(paste0("The following Stressor_Overall records are missing a ranking where an individual stressor was recorded:",
                      "\n",
              paste0(miss_overall[, c("Code", "Year", "Visit_ID", "Stressor_Category",
                                      "Severity_Indiv", "Severity_Overall")], collapse = "\n ")))}

    miss_indiv <- tbl_RAM_stress1 |> group_by(Code, Year, Visit_ID, Stressor_Category) |>
      summarize(num_indiv_stress = sum(Severity_Indiv > 0),
                stress_overall = sum(Severity_Overall > 0), .groups = 'drop') |>
      filter(num_indiv_stress == 0 & stress_overall > 0)

    if(nrow(miss_indiv) > 0){
      warning(paste0("The following Stressor_Category records are have an Overall ranking without an individual stressor recorded:",
                     "\n",
              paste0(miss_indiv[, c("Code", "Year", "Visit_ID", "Stressor_Category")], collapse = "\n ")))}

    # Alterations to Hydro Period and Stressors to Substrate don't have an overall score.
    # Applying max Indiv per group to Overall
    tbl_RAM_stress1 <- tbl_RAM_stress1 |> group_by(Code, Location_ID, Visit_ID, Panel, Date, Year,
                                                   Visit_Type, limited_RAM, Location_Level, Stressor_Category) |>
      mutate(Severity_Overall = ifelse(
        Stressor_Category %in% c("Alterations to Hydroperiod", "Stressors to Substrate"),
        max(Severity_Indiv, na.rm = T), Severity_Overall)) |>
      ungroup()

    stress_check <- tbl_RAM_stress1 |>
      group_by(Code, Location_ID, Visit_ID, Panel, Date, Year,
               Visit_Type, limited_RAM, Location_Level, Stressor_Category) |>
      mutate(check_indiv = ifelse(max(Severity_Indiv, na.rm = T) > max(Severity_Overall), 1, 0)) |>
      ungroup() |>
      filter(check_indiv > 0) |>
      select(Code, Year, Visit_ID, Stressor, Stressor_Category, Severity_Indiv, Severity_Overall)

    if(nrow(stress_check) > 0){warning(
    paste0("The following records have an overall severity less than the highest recorded individual severity:",
           paste0(stress_check, collapse = "\n "))
                                       )}
    #table(tbl_RAM_stress1$Stressor_Category, tbl_RAM_stress1$Severity_Overall, useNA = 'always')

    #-- prepare and add in xref_Visit_Hydrologic_Stressor
    tbl_hstress1 <- left_join(xref_Visit_Hydrologic_Stressor, tlu_Hydrologic_Stressor,
                             by = c("Hydrologic_Stressor_ID", "Hydrologic_Stressor_Category_ID"))

    tbl_hstress2 <- right_join(tbl_visits[,first_cols], tbl_hstress1, by = c("Visit_ID" = "Visit_Id"))
    tbl_hstress2$Location_Level = "AA"
    tbl_hstress2$Stressor_Category = "Hydrological"
    tbl_hstress2$Stressor = tbl_hstress2$Hydrologic_Stressor
    tbl_hstress2$Severity_Indiv = tbl_hstress2$Rank
    # Only have indiv ranks for each hydro stressor. Like with Alterations to Hydroperiod,
    # will take the max of each visit as Overall
    tbl_hstress2 <- tbl_hstress2 |>
      group_by(Code, Location_ID, Visit_ID, Panel, Date, Year,
               Visit_Type, Location_Level, Stressor_Category) |>
      mutate(Severity_Overall = max(Severity_Indiv, na.rm = T)) |>
      ungroup()

    tbl_hstress3 <- tbl_hstress2[, names(tbl_RAM_stress1)]

    tbl_RAM_stressors <- rbind(tbl_RAM_stress1, tbl_hstress3) |>
      arrange(Code, Year, Location_Level, Stressor_Category) |>
      filter(Severity_Indiv > 0)

    setTxtProgressBar(pb, length(tbl_list) + 3)
    close(pb)

  # Remove protected species if specified
  if(export_protected == FALSE){
    num_spp_prot <- filter(tbl_species_list, Protected_species == TRUE)
    num_spp2_prot <- filter(tbl_species_by_strata, Protected_species == TRUE)

    spp_drops <- data.frame(table(num_spp_prot$Latin_Name))

    # Handling adding Latin_Name column if df is empty
    if(nrow(spp_drops) > 0){
      colnames(spp_drops) <- c("Latin_Name", "Num_Sites")
    } else if(nrow(spp_drops) == 0){
    spp_drops <- data.frame("Latin_Name" = NA, "Num_Sites" = NA)
    spp_drops <- spp_drops[0,]
    }

    prot_mess <- paste0("Protected species were removed from this export, with ", nrow(num_spp_prot),
                   " records removed from tbl_species_list, and ", nrow(num_spp2_prot),
                   " records removed from tbl_species_by_strata. Species removed from tbl_species_list were: ",
                   paste0(spp_drops$Latin_Name, " (", spp_drops$Num_Sites, ")", collapse = "; "))

    if(nrow(spp_drops) > 0){
    cat(paste0("\033[0;", 31, "m", prot_mess, "\033[0m","\n"))
    }

    tbl_species_list <- filter(tbl_species_list, Protected_species == FALSE)
    tbl_species_by_strata <- filter(tbl_species_by_strata, Protected_species == FALSE)
  } else {
    prot_mess = "Protected species are included in views. These are for internal or NPS approved use only."
    cat(paste0("\033[0;", 31, "m", prot_mess, "\033[0m","\n"))
        }


  # final tables to add to new env or global env and print to disk
  final_tables <- list(tbl_locations, tbl_visits, tbl_visit_history, tbl_RAM_stressors,
                       tbl_AA_char, tbl_species_list, tbl_species_by_strata, tbl_vertical_complexity,
                       tlu_Plant)

  final_tables <- setNames(final_tables,
                           c("locations", "visits", "visit_history", "RAM_stressors",
                             "AA_char", "species_list", "species_by_strata", "vertical_complexity",
                             "tlu_Plant"))

  list2env(final_tables, envir = env)
  }


  if(type == "csv"){
  # List csvs in filepath folder
  dp_list <- list.files(filepath, pattern = ".csv")
  # Drop csvs that don't matching names in the views
  dp_list <- dp_list[grepl(paste0(views, collapse = "|"), dp_list)]
  # Drop date stamp (if it exists) from file name if exists in 2 steps
  dp_list_names <- gsub("[[:digit:]]+|.csv", "", dp_list)
  dp_list_names <- gsub("_$","", dp_list_names)

  miss_vws <- setdiff(dp_list_names, views)

  # Check for missing views
  if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                paste0(miss_vws, collapse = ", "))}

  if(length(dp_list) > 9){
    stop(
      "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'visits.csv'). Must specify a filepath that only contains 1 version of each view.")
  }

  # Setup progress bar
  pb <- txtProgressBar(min = 0, max = length(dp_list), style = 3)

  # Import the file names by applying read.csv to the dp_list of file names
  # This will return one list that includes all the datasets as individual elements
  # The na.string = NA converts "NA" in data to blanks. The check.names = F won't
  # replace invalid characters (eg "+") with "."
  final_tables <- lapply(seq_along(dp_list),
                     function(x){
                       fname = dp_list[[x]]
                       setTxtProgressBar(pb, x)
                       read.csv(paste0(filepath, fname),
                                na.string = "NA",
                                tryLogical = TRUE,
                                check.names = FALSE)
                     })

  # Set the names of dp_files as the shorter dp_list2 names
  final_tables <- setNames(final_tables, dp_list_names)

  # Takes every element of the dp_files list and saves it to the VIEWS_WQ or global
  # environment as separate, named objects.
  list2env(final_tables, envir = env)

  # Close progress bar
  close(pb)

  }

  if(type == "zip"){
  # Check if can read files within the zip file
    tryCatch(
      {zfiles = utils::unzip(filepath, list = T)$Name
      },
      error = function(e){stop(paste0("Unable to import specified zip file."))})

  z_list = zfiles[grepl(paste0(views, collapse = "|"), zfiles)]

  # Drop date stamp (if it exists) from file name if exists in 2 steps
  z_list_names <- gsub("[[:digit:]]+|.csv", "", z_list)
  z_list_names <- gsub("./", "", z_list_names)
  z_list_names <- gsub("_$","", z_list_names)

  miss_vws <- setdiff(z_list_names, views)

  # Check for missing views
  if(length(miss_vws) > 0){stop("Missing the following views from the specified filepath: ",
                                paste0(miss_vws, collapse = ", "))}

  if(length(z_list) > 11){
    stop(
      "More than one file matching the data package names were detected in the specified filepath
    (e.g. 'Chemistry_Data'). Must specify a filepath that only contains 1 version of each view.")
  }

  # Since the missing test passed, clean up files so only includes names in view_list, but
  # maintain order in files

  # Import views now that all tests passed
  pb <- txtProgressBar(min = 0, max = length(z_list), style = 3)

  ramviews <- unzip(filepath, junkpaths = TRUE, exdir = tempdir())

  final_tables <-
    lapply(seq_along(ramviews), function(x){
      setTxtProgressBar(pb,x)
      read.csv(ramviews[x], na.string = "NA", check.names = FALSE)})

  final_tables <- setNames(final_tables, z_list_names)
  list2env(final_tables, envir = env)
  # Close progress bar
  close(pb)

  }

  prot <- ifelse(export_protected == TRUE, "_NPSonly", "_public")

  if(export_data == TRUE){
    if(export_protected == FALSE){
      num_spp_prot <- filter(final_tables$species_list, Protected_species == TRUE)
      num_spp2_prot <- filter(final_tables$species_by_strata, Protected_species == TRUE)

      spp_drops <- data.frame(table(num_spp_prot$Latin_Name))

      # Handling adding Latin_Name column if df is empty
      if(nrow(spp_drops) > 0){
        colnames(spp_drops) <- c("Latin_Name", "Num_Sites")
      } else if(nrow(spp_drops) == 0){
        spp_drops <- data.frame("Latin_Name" = NA, "Num_Sites" = NA)
        spp_drops <- spp_drops[0,]
      }

      prot_mess <- paste0("Protected species were removed from this export, with ", nrow(num_spp_prot),
                          " records removed from tbl_species_list, and ", nrow(num_spp2_prot),
                          " records removed from tbl_species_by_strata. Species removed from tbl_species_list were: ",
                          paste0(spp_drops$Latin_Name, " (", spp_drops$Num_Sites, ")", collapse = "; "))

      if(nrow(spp_drops) > 0){
        cat(paste0("\033[0;", 31, "m", prot_mess, "\033[0m","\n"))
      }

      final_tables$species_list <- dplyr::filter(final_tables$species_list, Protected_species == FALSE)
      final_tables$species_by_strata <- dplyr::filter(final_tables$species_by_strata, Protected_species == FALSE)
    } else {
      prot_mess = "Protected species are included in views. These are for internal or NPS approved use only."
      cat(paste0("\033[0;", 31, "m", prot_mess, "\033[0m","\n"))
    }

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

      zip::zipr(zipfile = paste0(export_pathn, "NETN_Wetland_RAM_Data_", format(Sys.Date(), "%Y%m%d"), prot, ".zip"),
                root = tmp,
                files = file_list)
      # csvs will be deleted as soon as R session is closed b/c tempfile
    }
  }

  end_mess1 <- "Data package complete. Views are located in VIEWS_RAM environment. "
  end_mess2 <- "Data package complete. Views are located in global environment. "

  if(export_data == FALSE){
    if(new_env == TRUE){print(end_mess1)
    } else if(new_env == FALSE){print(end_mess2)}
  } else if(export_data == TRUE){
    end_mess3 <- paste0("Files saved to: ", export_pathn, " ")
    end_mess4 <- paste0("Zip file saved to: ", export_pathn,
                        "NETN_Wetland_RAM_Data_", format(Sys.Date(), "%Y%m%d"), prot, ".zip ")

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





