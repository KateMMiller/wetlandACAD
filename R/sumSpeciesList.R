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
#'
#' @return Returns a data frame with vegetation MMIs for each site.
#' @export

sumSpeciesList <- function(site = "all", panel = 1:4, years = 2012:format(Sys.Date(), "%Y"),
                           QAQC = FALSE, species_type = "all"){

  #---- Error Handling ----
  site_list <- paste0("R-", sprintf("%02d", 1:40))
  site <- match.arg(site, c("all", site_list), several.ok = TRUE)
  site <- if(any(site == "all")){site_list} else {site}

  stopifnot(class(panel) %in% c("numeric", "integer"), panel %in% c(1, 2, 3, 4))
  stopifnot(class(years) %in% c("numeric", "integer"), years >= 2012)
  stopifnot(class(QAQC) == "logical")
  species_type <- match.arg(species_type, c("all", "native", "exotic"))

  #---- Compile Data ----
  env <- if(exists("VIEWS_RAM")){VIEWS_RAM} else {.GlobalEnv}

  spplist <- tryCatch(get("species_list", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type",
                                                              "limited_RAM", "CoC_ME_ACAD", "TSN", "Latin_Name")],
                      error = function(e){stop("The tbl_species_list table was not found. Please import wetland RAM views.")}
                      )
  visit <- get("visits", envir = env)[,c("Code", "Location_ID", "Visit_ID", "Panel", "Date", "Year", "Visit_Type", "limited_RAM",
                                             "Bryophyte_Cover", "Invasive_Cover")]
  loc <- get("locations", envir = env)[,c("Code", "Location_ID", "Panel", "xCoordinate", "yCoordinate", "UTM_Zone")]



  if(nrow(vmmi_qaqc) == 0){
    stop("Arguments returned a data frame with no records. Be sure you specified RAM years, and not EPA NWCA years.")}

  vmmi_final <- left_join(vmmi_qaqc, loc, by = c("Code", "Location_ID", "Panel")) |>
    select(Code, Location_ID, Visit_ID, Panel, xCoordinate, yCoordinate, UTM_Zone, Date, Year, Visit_Type,
           limited_RAM, meanC, Bryophyte_Cover, Invasive_Cover, Cover_Tolerant = sum_cov_tol,
           vmmi, vmmi_rating)

  return(vmmi_final)

  }
