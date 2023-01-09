#' @title NWCA16_import: Import NWCA 2016 data from file
#'
#' @description This function imports all data files from the NWCA2016 from a zip or individual files as data frames. Metadata files are not imported. Each data frame is either added to the global environment or to an environment named NWCA16, based on whether new_env = TRUE or FALSE.
#'
#' @importFrom dplyr arrange filter mutate
#'
#' @param path Quoted path of folder containing data files.
#'
#' @param new_env Logical. Specifies which environment to store data frames in. If \code{TRUE}(Default), stores
#' data frames in NWCA16 environment. If \code{FALSE}, stores data frames in global environment
#'
#' @param zip_name Quoted string ending in .zip. If specified, function looks for the specified file name and
#' imports .csvs from the zip file. If not specified, function looks for and imports individual csvs. Note that
#' this takes slightly longer than loading individual .csvs, due to the unzipping process.
#'
#' @param ACAD_only Logical. If TRUE, only imports ACAD sentinel sites. If FALSE, imports full dataset.
#'
#' @return NWCA16 data files as data frames in specified environment
#'
#' @examples
#' \dontrun{
#'
#' # Import using default of adding new environment, not using zip file, and including only ACAD sites.
#' NWCA16_import(path = "../data/NWCA16")
#'
#' # Import all sites from zip file into global environment
#' NWCA16_import(path = "../data/NWCA16", new_env = FALSE,
#'               zip_name = "NWCA2016_data.zip", ACAD_only = FALSE)
#'
#' }
#'
#' @export

NWCA16_import<- function(path = NA, new_env = TRUE, zip_name = NA, ACAD_only = TRUE){

  #----- Error handling -----
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(ACAD_only) == 'logical')

  # Error handling for path
  if(is.na(path)){stop("Must specify a path to import csvs.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  # Make sure zip file exists and all the dfs are included
  if(!is.na(zip_name)){
    if(!file.exists(paste0(path, zip_name))){stop("Specified zip file doesn't exist in path.")}}

  options(scipen = 100) # For TSNs

  data_list <- sort(c("algal_toxin", "buffer_natcov", "buffer_stress", "hydro_sources", "hydro_stress",
                 "hydro_indicators", "site_info", "site_AAchar", "soil_chem_hor", "soil_chem_std",
                 "soil_prof_pit", "soil_prof_hor", "veg_data", "veg_tree", "veg_type", "veg_surf",
                 "veg_layout", "veg_plot_loc", "water_char", "water_chem"))

  # Make sure all the dfs are in the path or zip file. If anything is missing, function stops.
  files <-
    if(!is.na(zip_name)){
      zfiles <- unzip(paste0(path, zip_name), list = TRUE)$Name
      files <- substr(zfiles, 1, nchar(zfiles) - 4)
    } else if(is.na(zip_name)) {
      files <- substr(list.files(path), 1, nchar(list.files(path)) - 4)}

  missing <- setdiff(data_list, files)

  if(length(missing) > 0 & length(missing) < length(data_list)){
    stop(paste0("Missing the following data files: ", paste0(missing, collapse = ", ")))
  } else if (length(missing) == length(data_list)){
    stop(paste0("Data files were not detected in specified ", ifelse(is.na(zip_name), "path.", "zip file.")))}

  #----- Import dfs now that all tests passed -----
  # Clean up files so only includes names in data_list, but maintain order in files
  files <- sort(intersect(files, data_list))

  pb <- txtProgressBar(min = 0, max = length(data_list), style = 3)

  # Importing data
  data_import <-
    if(!is.na(zip_name)){
      dfs <- unzip(paste0(path, zip_name), junkpaths = TRUE, exdir = tempdir())
      lapply(seq_along(data_list), function(x){
        setTxtProgressBar(pb,x)
        read.csv(dfs[x])})
    } else if(is.na(zip_name)){
      lapply(seq_along(data_list), function(x){
        setTxtProgressBar(pb, x)
        read.csv(paste0(path, data_list[x], ".csv"))
      })
    }

  data_import <- setNames(data_import, files)

  if(ACAD_only == TRUE){

  ACAD_sites <-  data.frame(SITE_ID =
                              c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                                "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310"),
                            LOCAL_ID =
                              c("DUCK", "WMTN", "BIGH", "GILM", "LIHU",
                                "NEMI", "GRME", "HEBR", "HODG", "FRAZ"))

  data_import <-
    lapply(seq_along(data_import), function(x){
    df <- data_import[[x]] |> filter(SITE_ID %in% ACAD_sites$SITE_ID)
    df <- left_join(df, ACAD_sites, by = "SITE_ID")
    return(df)
    })

  data_import <- setNames(data_import, files)

  } else {data_import}

  if(new_env == TRUE){
    NWCA16 <<- new.env()
    list2env(data_import, envir = NWCA16)
  } else {
    list2env(data_import, envir = .GlobalEnv)}

  close(pb)

  print(ifelse(new_env == TRUE, paste0("Import complete. Data files are located in NWCA16 environment."),
               paste0("Import complete. Data files are located in global environment.")), quote = FALSE)

}


