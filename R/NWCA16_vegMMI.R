#' @title NWCA16_vegMMI: calculate Vegetation Multi-metric Index
#'
#' @description This function calculates the 4 metrics used to calculate the vegetation multi-metric index (MMI) developed by Miller et al. (2016). Note that to calculate the MMI, the species data must be joined with the Coefficient of Conservatism ranking. This is only implemented for EcoRegion 82 and downloads the CoCs from the Maine Natural Areas Program's website: https://www.maine.gov/dacf/mnap/features/coc.htm. This function is only implemented for ACAD sites and will break if the url hosting the CoC rankings changes.
#'
#' @importFrom dplyr arrange filter first group_by mutate rename select summarize
#'
#' @param export_spp Logical. If TRUE, adds a dataframe called veg_spp_cov that contains average percent cover of all species recorded at a site to the global environment. If FALSE (default), only returns the final VMMI dataframe.
#'
#' @return Data frame with vegetation MMI metrics and ratings
#'
#' @examples
#' \dontrun{
#'
#' # Import ACAD only data and export
#' NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
#' vmmi <- NWCA16_vegMMI(export_spp = TRUE)
#'
#' }
#'
#' @export

NWCA16_vegMMI<- function(export_spp = FALSE){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("readxl", quietly = TRUE)){
    stop("Package 'readxl' needed for this function to work. Please install it.", call. = FALSE)
  }

  # Error handling
  stopifnot(class(export_spp) == 'logical')

  # Extract veg data
  env = if(exists("NWCA16")){NWCA16} else {.GlobalEnv}

  tryCatch(
    {vegcov1 <- get("veg_data", envir = env) |>
                 dplyr::select(UID, SITE_ID, YEAR, VISIT_NO, DATE_COL, PLOT,
                        SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW)},
     error = function(e){stop("The veg_data data frame was not found. Please import the data.")})

  # filter on ACAD sites and add local name
    ACAD_sites <-  data.frame(SITE_ID =
                                c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                                  "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310"),
                              LOCAL_ID =
                                c("DUCK", "WMTN", "BIGH", "GILM", "LIHU",
                                  "NEMI", "GRME", "HEBR", "HODG", "FRAZ"))


     vegcov2 <- vegcov1 |> dplyr::filter(SITE_ID %in% ACAD_sites$SITE_ID)
     vegcov <- dplyr::left_join(vegcov2, ACAD_sites, by = "SITE_ID")

  # Importing COC table from MNAP website
  tryCatch(
    {url <- "https://www.maine.gov/dacf/mnap/features/Maine_CoC.xlsx"
     httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
     coc <- readxl::read_excel(tf) |>
       dplyr::mutate(SPECIES = toupper(`Scientific Name`)) |>
       dplyr::select(acronym, "PLANTS_Accepted Symbol", "SPECIES",
              nativity, "physiognomy", "duration",
              ecoreg_82_COC, "coefficient of wetness")},
     error = function(e){stop(paste0("Unable to download data from MNAP link: ", url))})

  # Importing surface data for percent bryophytes
  tryCatch(
    {vegtype = get("veg_type", envir = env) |>
      dplyr::select(UID, SITE_ID, PLOT, BRYOPHYTES) |>
      dplyr::filter(SITE_ID %in% ACAD_sites$SITE_ID)},
     error = function(e){stop("The veg_type data frame was not found. Please import the data.")})

  # Join COC to vegcov then check if there are species missing a coefficient
  vegcov_check <- left_join(vegcov, coc |> select("SPECIES", "ecoreg_82_COC", "nativity"),
                            by = "SPECIES")

  # Check for species without CoCs, and manually add in. Where a genus is recorded, use the average C of species
  # in that genus that are native.
  spp_miss <- vegcov_check |> filter(is.na(ecoreg_82_COC)) |>
    select(SPECIES, ecoreg_82_COC, nativity) |> unique()

  # Genus level scores (following NJNHP approach to filling in)
  AMELSPP <- coc |> filter(grepl("AMELANCHIER", SPECIES)) |> summarize(coc = mean(ecoreg_82_COC)) |> unique()
  VIOLA <- coc |> filter(grepl("VIOLA", SPECIES)) |> filter(nativity == "native") |>
    summarize(coc = mean(ecoreg_82_COC)) |> unique()

  addspp <- data.frame(SPECIES = c("AMELANCHIER", "VIOLA", "OCLEMENA xBLAKEI", "BETULA PAPYRIFERA",
                                   "VIBURNUM NUDUM", "ALNUS INCANA", "RUBUS IDAEUS", "SPARGANIUM EMERSUM"),
                       ecoreg_82_COC = as.numeric(c(AMELSPP, VIOLA, 6, 3,
                                         5, 3, 2, 3)),
                       nativity = c("native", "native", "native", "native",
                                    "native", "native", "native", "native"),
                       PLANTS = c("AMELA", "VIOLA", "OCBL", "BEPA",
                                  "VINU", "ALIN2", "RUID", "SPEM2"))
    # OCLxBLA = average of OCLACU (4) and OCLNEM (8)
    # BETPAP On CoC list as BETPAP var. PAP
    # VIBNUD On CoC list as VIBNUD var. CAS
    # ALNINC On CoC list as ALNINC var. RUG
    # RUBIDA CoC list includes 2 vars, but both are 2.
    # Missing from new CoC list; taken from prev. version of list.

  coc2 <- rbind(coc |> select(SPECIES, ecoreg_82_COC, nativity, PLANTS = `PLANTS_Accepted Symbol`),
                addspp)

  vegcov_coc <- left_join(vegcov, coc2, by = c("SPECIES")) |> rename(coc = ecoreg_82_COC) |>
    mutate(stress_tol = ifelse(coc > 0 & coc <= 4, 1, 0))

  # Calculate site-level average cover, mean C, and cover of stress tolerant plants (CoC > 0 and <=4):
  # The data provided by EPA is expanded, so that every species found at a site has a cover value,
  # including 0, for each of the 5 plots. That means summarizing can assume 5 plots. But, just in case,
  # I'll first check that every species is listed in every plot, rather than only when present:
  vegcheck <- vegcov_coc |> group_by(SITE_ID, SPECIES) |>
    summarize(num_plots = sum(!is.na(COVER)), .groups = "drop") |>
    filter(num_plots < 5)

  if(nrow(vegcheck) > 0){warning(
    paste0("The following species were missing at least one percent cover value among 5 plots: "),
      paste0(vegcheck$SPECIES, collapse = ", "))}

  # Summarize site by species
  vegsum <- vegcov_coc |> group_by(UID, SITE_ID, LOCAL_ID, SPECIES, SPECIES_NAME_ID, PLANTS, nativity) |>
    summarize(avgcov = mean(COVER),
              coc = first(coc),
              stress_tol = first(stress_tol),
              .groups = 'drop')

  if(export_spp == TRUE){assign( "veg_spp_cov", vegsum, envir = .GlobalEnv)}

  vmmi1 <- vegsum |> group_by(UID, SITE_ID, LOCAL_ID) |>
                     summarize(mean_c = mean(coc),
                               cov_tol = sum(avgcov[stress_tol == 1]),
                               cov_inv = sum(avgcov[nativity == "non-native"]),
                               .groups = 'drop')

  # Summarize average % Byrophyte from vegtype data frame
  # Check that every plot has a value first
  vegt_check <- vegtype |> group_by(SITE_ID) |>
    summarize(num_plots = sum(!is.na(BRYOPHYTES)), .groups = "drop") |>
    filter(num_plots < 5)

  if(nrow(vegt_check) > 0){warning(
    paste0("The following sites were missing at least one Bryophyte percent cover value among 5 plots: "),
    paste0(vegt_check$SITE_ID, collapse = ", "))}

  vegtype_sum <- vegtype |> group_by(UID, SITE_ID) |>
    summarize(cov_bryo = mean(BRYOPHYTES), .groups = 'drop')

  # Join datasets and calc. VMMI based on Miller et al. 2017., which requires adjusting
  # the floor and ceiling for each metric

  vmmi <- full_join(vmmi1, vegtype_sum, by = c("UID", "SITE_ID")) |>
    mutate(mean_c_adj = ((mean_c - 3.015)/(7.346 - 3.015)) * 10,
           cov_tol_adj = ((((cov_tol-0.386)/(136.645 - 0.386)) * 10) - 10) * -1, # reverse the scale
           cov_inv_adj = (((cov_inv/38.45) * 10) - 10) * -1,
           cov_bryo_adj = (cov_bryo/98.48) * 10,
           vmmi1 = mean_c_adj + cov_tol_adj + cov_inv_adj + cov_bryo_adj,
           vmmi = ((vmmi1 - 0.389)/(40 - 0.389)) * 100,
           vmmi_rank = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair"))
    ) |> arrange(SITE_ID)

  return(vmmi)

  }
