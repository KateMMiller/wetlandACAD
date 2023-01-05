#' @title NWCA16_import: Import NWCA 2016 data from file
#'
#' @description This function imports all data files from the NWCA2016 from a zip or individual files as data frames. Metadata files are not imported. Each data frame is either added to the global environment or to an environment named NWCA16, based on whether new_env = TRUE or FALSE.
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
#' @return NWCA16 data files as data frames in specified environment
#'
#' @examples
#' \dontrun{
#' # Import individual csvs into global environment
#' NWCA16_import(path = "./data/", new_env = FALSE)
#'
#' # Import zipped csvs into NWCA16 environment
#' NWCA16_import(path = "./data/", zip_name = "NWCA2016_data.zip")
#' }
#'
#' @export

NWCA16_import<- function(path = NA, new_env = TRUE, zip_name = NA){

  # Error handling for path
  if(is.na(path)){stop("Must specify a path to import csvs.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  options(scipen = 100) # For TSNs

  data_list <- c("algal_toxin", "buffer_natcov", "buffer_stress", "hydro_sources", "hydro_stress",
                 "hydro_indicators", "site_info", "site_AAchar", "soil_chem_hor", "soil_chem_std",
                 "soil_prof_pit", "soil_prof_hor", "veg_data", "veg_tree", "veg_type", "veg_surf",
                 "veg_layout", "veg_plot_loc", "water_char", "water_chem")

  # Make sure zip file exists and all the dfs are included
  if(!is.na(zip_name)){
    if(!file.exists(paste0(path, zip_name))){stop("Specified zip file doesn't exist in path.")}}

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

  # Since the missing test passed, clean up files so only includes names in data_list, but
  # maintain order in files
  files <- intersect(files, data_list)

  # Import dfs now that all tests passed
  pb <- txtProgressBar(min = 0, max = length(data_list), style = 3)

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

  if(new_env == TRUE){
    NWCA16 <<- new.env()
    list2env(data_import, envir = NWCA16)
  } else {
    list2env(data_import, envir = .GlobalEnv)}

  close(pb)

  print(ifelse(new_env == TRUE, paste0("Import complete. Data files are located in NWCA16 environment."),
               paste0("Import complete. Data files are located in global environment.")), quote = FALSE)

}


