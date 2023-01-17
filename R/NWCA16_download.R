#' @title NWCA16_download: Downloads data from 2016 EPA National Wetland Condition Assessment
#'
#' @description This function downloads the 2016 EPA NWCA data directly from the National Aquatic Resource Surveys website and saves the dataset to disk.
#'
#' @param path Quoted path of folder to download files into. If not specified, will save to working directory.
#'
#' @param zip Logical. If TRUE, exports a zip file. If FALSE (Default), exports individual csvs.
#'
#' @examples
#' \dontrun{
#' # Run with defaults: path = getwd(), zip = T
#' NWCA16_download()
#'
#' # Save files to specified data folder and not zipped
#' NWCA16_download(path = "./data", zip = FALSE)
#'
#'}
#' @return Writes downloaded files to disk.
#'
#' @export
#'

NWCA16_download <- function(path = NA, zip = TRUE){

  stopifnot(class(zip) == 'logical')

  # Check that suggested package required for this function are installed
  if(!requireNamespace("zip", quietly = TRUE) & zip == TRUE){
    stop("Package 'zip' needed to export to zip file. Please install it.", call. = FALSE)
  }

  options(scipen = 100) # for Taxa ID numbers

  # Error handling for path
  if(is.na(path)){
    path <- getwd()
    print(paste0("No path specified. Saving output to working directory: ", getwd()), quote = FALSE)
  } else if(!dir.exists(path)){
    stop("Specified directory does not exist.")
  } else {}

  # Normalize path for zip::zipr
  pathn <- normalizePath(path)

  # Add / to end of path if it wasn't specified.
  pathn <- if(substr(pathn, nchar(pathn), nchar(pathn)) != "/"){
    paste0(pathn,"\\")} else {(paste0(pathn))}

  # Download the data csvs. Note that the "-" in the file names causes issues in paste.
  # Had to use the full path and file.path to handle this.

  data_list <- c("algal_toxin", "buffer_natcov", "buffer_stress", "hydro_sources", "hydro_stress",
                 "hydro_indicators", "site_info", "site_AAchar", "soil_chem_hor", "soil_chem_std",
                 "soil_prof_pit", "soil_prof_hor", "veg_data", "veg_tree", "veg_type", "veg_surf",
                 "veg_layout", "veg_plot_loc", "water_char", "water_chem")

  metadata_list <- paste(data_list, "meta", sep = "_")

  cat("Downloading data and metadata", "\n")

  pb <- txtProgressBar(min = 0, max = length(data_list) * 2, style = 3)
  x <- 0

  # Import csvs from website
  import_csv <- function(path){
    df <- read.csv(file.path(path))
    x <<- x+1 # for progress bar
    return(df)
    }

  algal_toxin <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_microcystin_-_data_csv.csv")

  setTxtProgressBar(pb, x)

  buffer_natcov <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_buffer_characterization_natcover_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  buffer_stress <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_buffer_characterization_stressors_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  hydro_sources <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_sources_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  hydro_stress <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_stressors_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  hydro_indicators <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_usacoe_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  site_info <- import_csv(
    "https://www.epa.gov/system/files/other-files/2022-04/nwca-2016-site-information-data_0.csv")
  setTxtProgressBar(pb, x)

  site_AAchar <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_characterization_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  soil_chem_hor <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_horizon_chemistry_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  soil_chem_std <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_stddepth_core_chemistry_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  soil_prof_pit <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_pit_characteristics_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  soil_prof_hor <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_horizon_description_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  veg_data <- import_csv(
    "https://www.epa.gov/system/files/other-files/2022-04/nwca-2016-plant-species-cover-height-data.csv")
  setTxtProgressBar(pb, x)

  veg_tree <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_tree_cover_count_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  veg_type <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_vegetation_type_-_data_csv_0.csv")
  setTxtProgressBar(pb, x)

  veg_surf <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_ground_surface_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  veg_layout <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_floras_used_and_veg_plot_layout_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  veg_plot_loc <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_veg_plot_location_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  water_char <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_surface_water_characterization_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  water_chem <- import_csv(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_water_chemistry_chla_-_data_csv.csv")
  setTxtProgressBar(pb, x)

  # Download metadata txt files
  # Import txts from website

  import_txt <- function(path){
    metadf <- read.delim(file.path(path))
    x <<- x+1
    return(metadf)
  }

  algal_toxin_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_microcystin_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  buffer_natcov_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_buffer_characterization_natcover_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  buffer_stress_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_buffer_characterization_stressors_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  hydro_sources_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_sources_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  hydro_stress_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_stressors_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  hydro_indicators_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_hydrology_usacoe_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  site_info_meta <- import_txt(
    "https://www.epa.gov/system/files/other-files/2022-04/nwca-2016-site-information-metadata_0.txt")
  setTxtProgressBar(pb, x)

  site_AAchar_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_aa_characterization_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  soil_chem_hor_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_horizon_chemistry_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  soil_chem_std_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_stddepth_core_chemistry_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  soil_prof_pit_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_pit_characteristics_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  soil_prof_hor_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_soil_horizon_description_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  veg_data_meta <- import_txt(
    "https://www.epa.gov/system/files/other-files/2022-04/nwca-2016-plant-species-cover-height-metadata.txt")
  setTxtProgressBar(pb, x)

  veg_tree_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_tree_cover_count_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  veg_type_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_vegetation_type_-_metadata_txt_0.txt")
  setTxtProgressBar(pb, x)

  veg_surf_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_ground_surface_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  veg_layout_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_floras_used_and_veg_plot_layout_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  veg_plot_loc_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_veg_plot_location_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  water_char_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_surface_water_characterization_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  water_chem_meta <- import_txt(
    "https://www.epa.gov/sites/default/files/2021-04/nwca_2016_water_chemistry_chla_-_metadata_txt.txt")
  setTxtProgressBar(pb, x)

  close(pb)

  # Check that all of the data frames successfully downloaded.
  missing_data <- setdiff(data_list, ls())

  if(length(missing_data) > 0){
    warning("The following data frames failed to download: \n",
            paste("\t", missing_data, "\n"))
  }

  # Check that all of the metadata files successfully downloaded.
  missing_metadata <- setdiff(metadata_list, ls())

  if(length(missing_metadata) > 0){
    warning("The following metadata files failed to download: \n",
            paste("\t", missing_metadata, "\n"))
  }

  cat("Writing files to disk")

  # # Export files
  if(zip == FALSE){
    invisible(lapply(seq_along(data_list), function(x){
      write.csv(get(data_list[[x]]),
                paste0(pathn, data_list[x], ".csv"),
                row.names = FALSE)
    }))

    invisible(lapply(seq_along(metadata_list), function(x){
      write.csv(get(metadata_list[[x]]),
                paste0(pathn, metadata_list[x], ".csv"),
                row.names = FALSE)
    }))

  } else if(zip == TRUE){ #create tmp dir to export csvs, bundle to zip, then delete tmp folder

    # Write data files to zip, first using temp file to save files to
    dir.create(tmp <- tempfile())

    invisible(lapply(seq_along(data_list), function(x){
      write.csv(get(data_list[[x]]),
                paste0(tmp, "\\", data_list[x], ".csv"),
                row.names = FALSE)}))

    file_list <- list.files(tmp)

    zip::zipr(zipfile = paste0(pathn, "NWCA2016_data.zip"),
              root = tmp,
              files = file_list)

    # Note that pathn is the normalized path with \\ at the end. Otherwise zip crashes

    # Write metadata to separate zip
    dir.create(tmp2 <- tempfile())
    invisible(lapply(seq_along(metadata_list), function(x){
      write.csv(get(metadata_list[[x]]),
                paste0(tmp2, "\\", metadata_list[x], ".csv"),
                row.names = FALSE)}))

    file_list2 <- list.files(tmp2)

    zip::zipr(zipfile = paste0(pathn, "NWCA2016_metadata.zip"),
              root = tmp2,
              files = file_list2)

  }

  cat("...Done")
}


