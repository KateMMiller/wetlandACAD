#' @title make_datapkg_RAM: Imports and compiles views for wetland RAM data package
#'
#' @description This function imports RAM-related tables in the wetland RAM backend and combines them into flattened views for the data package. Each view is added to a VIEWS_WETLAND environment in your workspace, or to your global environment based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr all_of arrange collect filter full_join group_by left_join mutate rename right_join summarize tbl
#' @importFrom purrr reduce
#' @importFrom tidyr pivot_wider
#'
#'@param export_protected Logical. If TRUE, all records are exported. If FALSE (Default), only non-protected
#'species are exported.
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
#' @param path Quoted path of database back end file, including the name of the backend.
#' @return Assigns RAM views to specified environment
#'
#' @param new_env Logical. Specifies which environment to store views in. If \code{TRUE}(Default), stores
#' views in VIEWS_RAM environment. If \code{FALSE}, stores views in global environment
#'
#' @examples
#' \dontrun{
#' # Import tables from database in specific folder:
#' make_datapkg_RAM(type = 'file', path = './Data/NETN_RAM_Backend.mdb')
#'
#' # Import ODBC named database into global env with protected species
#' make_datapkg_RAM(type = 'DSN', odbc = "RAM_BE", new_env = F, export_protected = T)
#' }
#'
#' @export

make_datapkg_RAM <- function(export_protected = FALSE,
                               type = c('DSN', 'file'), odbc = 'RAM_BE',
                               path = NA, new_env = TRUE){

  #---- error handling ----
  stopifnot(class(export_protected) == 'logical')
  type <- match.arg(type)
  stopifnot(class(new_env) == 'logical')

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
    if(is.na(path)){stop("Must specify a path to the database for type = file option.")
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
  tbl_list <- tbl_list1[!grepl("tbl_Well|tbl_Well_Visit|tbl_Water_Level", tbl_list1)] # drops well tbls and queries

  pb = txtProgressBar(min = 0, max = length(tbl_list), style = 3)

  tbl_import <- lapply(seq_along(tbl_list),
                       function(x){
                         setTxtProgressBar(pb, x)
                         tab1 <- tbl_list[x]
                         tab <- dplyr::tbl(db, tab1) |> dplyr::collect() |> as.data.frame()
                         return(tab)
                       })

  DBI::dbDisconnect(db)

  tbl_import <- setNames(tbl_import, tbl_list)

  if(new_env == TRUE){VIEWS_RAM <<- new.env()}
  env <- if(new_env == TRUE){VIEWS_RAM} else {.GlobalEnv}

  list2env(tbl_import, envir = environment()) # all tables into fxn env

  #---- Combine tables into views ----
    #--- tbl_locations
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
                                by = "Layout_ID")

    tbl_locations <- tbl_locations5[,c("Code", "Location_ID", "Panel", "Date_Established",
                                       "Contact_ID", "Easting", "Northing",
                                       "UTM_Zone", "Description", "FWS_Class_Code",
                                       "HGM_Class", "HGM_Sub_Class", "AA_Layout", "AA_Area",
                                       "Directions", "Location_Comments", "Access_Comments",
                                       "Notes_AA2", "Access_Difficulty", "Access_Requirement",
                                       "Wetland_Hydro_Comments")]
    tbl_locations <- arrange(tbl_locations, Code)

    #--- tbl_visits
    tbl_Visit <- tbl_Visit |> mutate(Year = substr(Date, 1, 4))

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

    first_cols <- c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type")
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

    #setdiff(names(tbl_visits6), names(tbl_visits)) # dropped unwanted names

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

    tbl_AA_char <- rbind(tbl_topo2, tbl_water2) |> arrange(Code, Year, Type, Feature)

    #--- tbl_species_list
    tbl_species1 <- left_join(xref_Species_List |> rename(TSN = Plant_ID),
                              tlu_Plant |> select(Accepted_Latin_Name, TSN_Accepted, TSN, Latin_Name, Common,
                                                  Order, Family, Genus, PLANTS_Code, CoC_ME_ACAD, ACAD_ED,
                                                  Exotic, Invasive, Aquatic, Fern_Ally, Graminoid, Herbaceous,
                                                  Moss_Lichen, Shrub, Tree, Vine, Synonym, Author,
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
                   "TSN_Accepted", "Accepted_Latin_Name", "Synonym", "Author", "Protected_species")

    tbl_species_list <- tbl_species2[,new_order]
    #setdiff(names(tbl_species2), names(tbl_species)) # check that dropped unwanted columns
    #head(tbl_species)

    #--- tbl_species_by_strata
    tbl_vert1 <- left_join(xref_Vert_Complexity, tlu_Vert_Complexity, by = "Vert_Complexity_ID")
    tbl_vert2 <- left_join(tbl_vert1, tlu_Strata, by = "Strata_ID")
    tbl_vert2$Vert_Complexity[tbl_vert2$Vert_Complexity_ID == 6] <- "0%"

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

    tbl_stress_overall <- tbl_stress3 |> filter(Stressor %in% "Overall Ranking") |> select(-Stressor, Stressor_ID_Overall = Stressor_ID)
    tbl_stress_indiv <- tbl_stress3 |> filter(!Stressor %in% "Overall Ranking")

    tbl_RAM_stress1 <- full_join(tbl_stress_overall, tbl_stress_indiv,
                                 by = c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                        "Location_Level", "Stressor_Category", "Stressor_Category_ID"),
                                 suffix = c("_Overall", "_Indiv")) |>
                       #filter(Severity_Indiv > 0) |>
                       select(all_of(first_cols), Location_Level, Stressor_Category, Stressor, Severity_Indiv, Severity_Overall)

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
                                                   Visit_Type, Location_Level, Stressor_Category) |>
      mutate(Severity_Overall = ifelse(
        Stressor_Category %in% c("Alterations to Hydroperiod", "Stressors to Substrate"),
        max(Severity_Indiv, na.rm = T), Severity_Overall)) |>
      ungroup()

    stress_check <- tbl_RAM_stress1 |>
      group_by(Code, Location_ID, Visit_ID, Panel, Date, Year,
               Visit_Type, Location_Level, Stressor_Category) |>
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

    tbl_RAM_stressors <- rbind(tbl_RAM_stress1, tbl_hstress3) |> arrange(Code, Year, Location_Level, Stressor_Category) |>
      filter(Severity_Indiv > 0)

  # Remove protected species if specified
  if(export_protected == FALSE){
    num_spp_prot <- filter(tbl_species_list, Protected_species == TRUE)
    num_spp2_prot <- filter(tbl_species_by_strata, Protected_species == TRUE)

    spp_drops <- data.frame(table(num_spp_prot$Latin_Name))
    colnames(spp_drops) <- c("Latin_Name", "Num_Sites")

    warning(paste0("Protected species were removed from this export, with ", nrow(num_spp_prot),
                   " records removed from tbl_species_list, and ", nrow(num_spp2_prot),
                   " records removed from tbl_species_by_strata. Species removed from tbl_species_list were: ",
                   paste0(spp_drops$Latin_Name, " (", spp_drops$Num_Sites, ")", collapse = "; ")))

    tbl_species_list <- filter(tbl_species_list, Protected_species == FALSE)
    tbl_species_by_strata <- filter(tbl_species_by_strata, Protected_species == FALSE)
   } else {warning("Note that protected species are included in views. These are for internal or NPS approved use only.")}

  close(pb)

  #final tables to add to new env or global env: tbl_locations, tbl_visits

  print(ifelse(new_env == TRUE,
        paste0("Import complete. Views are located in VIEWS_WETLAND environment."),
        paste0("Import complete. Views are located in global environment.")
        ))

  final_tables <- list(tbl_locations, tbl_visits, tbl_RAM_stressors,
                       tbl_AA_char, tbl_species_list, tbl_species_by_strata)

  final_tables <- setNames(final_tables,
                           c("tbl_locations", "tbl_visits", "tbl_RAM_stressors",
                             "tbl_AA_char", "tbl_species_list", "tbl_species_by_strata"))

  list2env(final_tables, envir = env)

  } # End of function





