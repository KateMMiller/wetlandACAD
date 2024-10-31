#' @title sumSpeciesList: compile species list for each site
#'
#' @importFrom dplyr filter group_by left_join mutate select summarize
#'
#' @description This function compiles species lists filtered on site, year, panel, QAQC status, and native vs exotic species.
#'
#' @param site Character. Filter on site code. Options are "all" (default) or a vector of site codes ranging from "R-01" to "R-40".
#' @param panel Numeric. Filter on panel number. By default, all panels are returned, and can be filtered by numbers 1 to 4.
#' @param years Numeric. Filter on sample year, ranging from 2012 to 2024. By default, all years are returned. Note that years 2011, 2016, 2021, and 5-year intervals of years thereafter are EPA NWCA sites that are not
#' @param QAQC Logical. Include QAQC visits (TRUE) or drop QAQC visits (FALSE; default).
#' @param species_type Character. Options are c("all", "native", "exotic")
#' @param include_protected Logical. If TRUE, returns protected species. If FALSE (default), removes protected species from returned data frame.
#'
#' @return Returns a data frame with species lists for each site.
#'
#' @examples
#' \dontrun{
#' # import RAM data with protected records- Note that to include protected records
#' # protected species records must be included
#' importRAM(export_protected = T)
#'
#' # Compile species list for all sites and all species
#' spp_all <- sumSpeciesList(include_protected = T)
#'
#' # Compile species list for all sites and only non-protected species
#' spp_public <- sumSpeciesList(include_protected = T)
#'
#' # Compile for 2024 and exotic species only
#' spp_exo_24 <- sumSpeciesList(years = 2024, species_type = "exotic")
#'
#' }
#'
#' @export

sumSpeciesList <- function(site = "all", panel = 1:4, years = 2012:format(Sys.Date(), "%Y"),
                           QAQC = FALSE, species_type = "all", include_protected = T){

  #---- Error Handling ----
  site_list <- paste0("R-", sprintf("%02d", 1:40))
  site <- match.arg(site, c("all", site_list), several.ok = TRUE)
  site <- if(any(site == "all")){site_list} else {site}

  stopifnot(class(panel) %in% c("numeric", "integer"), panel %in% c(1, 2, 3, 4))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2012)
  stopifnot(class(QAQC) == "logical")
  species_type <- match.arg(species_type, c("all", "native", "exotic"))
  stopifnot(class(include_protected) == "logical")

  #---- Compile Data ----
  env <- if(exists("VIEWS_RAM")){VIEWS_RAM} else {.GlobalEnv}

  spplist <- tryCatch(get("species_list", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                                              "limited_RAM", "TSN", "Latin_Name", "quad_freq")],
                      error = function(e){stop("The tbl_species_list table was not found. Please import wetland RAM views.")}
                      )
  visit <- get("visits", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type", "limited_RAM")]
  loc <- get("locations", envir = env)[,c("Code", "Location_ID", "Panel", "xCoordinate", "yCoordinate", "UTM_Zone")]

  locev <- left_join(loc, visit, by = c("Code", "Location_ID", "Panel"))

  spp_loc <- left_join(locev, spplist, by = c("Code", "Location_ID", "Panel", "Visit_ID",
                                              "Visit_Type", "limited_RAM", "Date", "Year"))
  spp_site <- filter(spp_loc, Code %in% site)
  spp_year <- filter(spp_site, Year %in% years)
  spp_panel <- filter(spp_year, Panel %in% panel)
  spp_qaqc <- if(QAQC == FALSE){filter(spp_panel, Visit_Type == "VS")} else {spp_panel}

  plants <- VIEWS_RAM$tlu_Plant
  spp_comb <- left_join(spp_qaqc, plants, by = c("TSN", "Latin_Name"))

  # species_type: Character. Options are c("all", "native", "exotic")
  spp_type <- switch(species_type,
                     "all" = spp_comb,
                     "native" = spp_comb |> filter(Exotic == FALSE),
                     "exotic" = spp_comb |> filter(Exotic == TRUE))

  spp_prot <- if(include_protected == TRUE){spp_type
  } else {filter(spp_type, Protected_species == FALSE)}

  if(nrow(spp_prot) == 0){
    stop("Arguments returned a data frame with no records. Be sure you specified RAM years, and not EPA NWCA years.")}

  spp_final <- spp_prot |>
    select(Code, Location_ID, Visit_ID, Panel, xCoordinate, yCoordinate, UTM_Zone, Date, Year, Visit_Type,
           limited_RAM, TSN, Latin_Name, Common, quad_freq, PLANTS_Code, CoC_ME_ACAD, Invasive, Protected_species, Coef_wetness)

  return(spp_final)

  }
