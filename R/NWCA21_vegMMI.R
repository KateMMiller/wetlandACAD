#' @title NWCA16_vegMMI: calculate Vegetation Multi-metric Index from local JSON
#'
#' @description This function calculates the 4 metrics used to calculate the vegetation multi-metric index (MMI) developed by Miller et al. (2016). Note that to calculate the MMI, the species data must be joined with the Coefficient of Conservatism ranking. This is only implemented for EcoRegion 82 and downloads the CoCs from the Maine Natural Areas Program's website: https://www.maine.gov/dacf/mnap/features/coc.htm. This function is only implemented for ACAD sites and will break if the url hosting the CoC rankings changes. This function imports the local copies of the JSON files exported from EPA's app during the NWCA2021 that are needed to calculate the vegetation MMI. Each data frame is either added to the global environment or to an environment named NWCA21, based on whether new_env = TRUE or FALSE. Note that these are preliminary data sets that have not been fully QCed by EPA, and only includes sites from ACAD. Once NWCA21 data are made public, this function will be updated to include final data sets.
#'
#' @importFrom dplyr arrange case_when filter group_by mutate select summarize
#' @importFrom tidyr everything fill pivot_longer pivot_wider
#' @importFrom purrr map_df map_dfr
#' @importFrom stringr str_extract str_replace
#'
#' @param path Quoted path of folder containing data files.
#'
#' @param export_spp Logical. If TRUE, adds a dataframe called veg_spp_cov that contains average percent cover of all species recorded at a site to the global environment. If FALSE (default), only returns the final VMMI dataframe.
#'
#' @param new_env Logical. Specifies which environment to store data frames in. If \code{TRUE}(Default), stores
#' data frames in NWCA21 environment. If \code{FALSE}, stores data frames in global environment
#'
#' @return Data frame with vegetation MMI metrics and ratings
#'
#' @examples
#' \dontrun{
#'
#' # Calculate vegMMI using local copies of veg forms in JSON format
#' NWCA21_vegMMI(path = "../data/NWCA21", new_env = FALSE)
#'
#' }
#'
#' @export

NWCA21_vegMMI<- function(path = NA, new_env = TRUE, export_spp = TRUE){

  #----- Error handling -----
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(export_spp) == 'logical')

  # Error handling for path
  if(is.na(path)){stop("Must specify a path to import csvs.")
  } else if(!dir.exists(path)){stop("Specified path does not exist.")}

  # Add / to end of path if it wasn't specified.
  path <- if(substr(path, nchar(path), nchar(path)) != "/"){paste0(path, "/")} else {(paste0(path))}

  # Check that suggested package required for this function are installed
  if(!requireNamespace("rjson", quietly = TRUE)){
    stop("Package 'rjson' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("readxl", quietly = TRUE)){
    stop("Package 'readxl' needed for this function to work. Please install it.", call. = FALSE)
  }

  options(scipen = 100) # For TSNs

  # Get site list
  files <- list.files(path, pattern = ".json")
  sites <- unique(substr(files, 1, 14))

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

  # Functions to read in V2 form
  # Import and compile V2 json
  read_v2_long <- function(path, site_name){
    df <- suppressWarnings(as.data.frame(rjson::fromJSON(file = paste0(path, "/", site_name, "_1_V-2.json")))[,-(1:9)] |>
                             pivot_longer(cols = everything(), names_to = 'header', values_to = 'value')  |>
                             mutate(veg_plot = str_replace(header, "^.+_(\\d+)_.+$", "\\1"), #deletes all but number b/t _##_
                                    veg_plot = as.numeric(veg_plot),
                                    row_num =  as.numeric(str_extract(header, "\\(?[0-9]+\\)?")), # extract first set of digits
                                    site_name = site_name) |>
                             filter(!is.na(veg_plot)))

    df$data_type <- case_when(grepl("_SPECIES", df$header) ~ paste0("Species"),
                              grepl("_COVER", df$header) ~ paste0("Cover", "_", df$veg_plot),
                              grepl("_NE", df$header) ~ paste0("NE", "_", df$veg_plot),
                              grepl("_SW", df$header) ~ paste0("SW", "_", df$veg_plot),
                              grepl("_HEIGHT", df$header) ~ paste0("Height", "_", df$veg_plot),
                              TRUE ~ NA_character_)

    df <- df |> mutate(species = ifelse(data_type == "Species", value, NA),
                        sort_order = ifelse(data_type == "Species", row_num,
                                            row_num + 0.1)) |>
      arrange(sort_order, data_type) |>
      tidyr::fill(species, .direction = "down") |> filter(data_type != "Species") |>
      select(-sort_order)
  }

  # Function to reshape v2 to wide
  read_v2_wide <- function(path, site_name){
    df <- read_v2_long(path, site_name)

    df2 <- df |> select(site_name, species, row_num, value, data_type) |>
      pivot_wider(names_from = data_type, values_from = value) |>
      arrange(species)

    return(df2)
  }

  # Read in V3 form for Bryophyte cover
  v3_bryo1 <- purrr::map_dfr(sites, function(site_name){
    as.data.frame(rjson::fromJSON(file = paste0(path, "/", site_name, "_1_V-3.json")))
     }) |>
    select(UID, SITE_ID, YEAR, bryo_p1 = V3.1_BRYOPHYTES,
           bryo_p2 = V3.2_BRYOPHYTES, bryo_p3 = V3.3_BRYOPHYTES,
           bryo_p4 = V3.4_BRYOPHYTES, bryo_p5 = V3.5_BRYOPHYTES)

  bryo_cols <- c("bryo_p1", "bryo_p2", "bryo_p3", "bryo_p4", "bryo_p5")
  v3_bryo1[, bryo_cols][is.na(v3_bryo1[, bryo_cols])] <- 0
  v3_bryo1$bryo_p1 <- as.numeric(v3_bryo1$bryo_p1)
  v3_bryo1$bryo_p2 <- as.numeric(v3_bryo1$bryo_p2)
  v3_bryo1$bryo_p3 <- as.numeric(v3_bryo1$bryo_p3)
  v3_bryo1$bryo_p4 <- as.numeric(v3_bryo1$bryo_p4)
  v3_bryo1$bryo_p5 <- as.numeric(v3_bryo1$bryo_p5)

  v3_bryo <- v3_bryo1 |>
    mutate(cov_bryo = (bryo_p1 + bryo_p2 + bryo_p3 + bryo_p4 + bryo_p5)/5)

  # Compile species list
  v2_all_sites <- purrr::map_df(sites, ~read_v2_wide(path, .x)) |>data.frame()
  cov_cols <- c("Cover_1", "Cover_2", "Cover_3", "Cover_4", "Cover_5")
  ht_cols <- c("Height_1", "Height_2", "Height_3", "Height_4", "Height_5")

  v2_all_sites$species[v2_all_sites$species == "MORELLA CAROLINIENSIS"] <- "MORELLA PENSYLVANICA"
  # Error in data b/c I thought M. caroliniensis was a syn. b/c M. pensylvanica wasn't an option.
  v2_all_sites$species[v2_all_sites$species == "LINNAEA BOREALIS SPP. LONGIFLORA"] <- "LINNAEA BOREALIS"
  # This is a western spp. Should just be L. borealis

  # Replace NAs with 0 and convert to numeric
  cover_cols <- c("Cover_1", "Cover_2", "Cover_3", "Cover_4", "Cover_5")
  v2_all_sites[, cover_cols][is.na(v2_all_sites[, cover_cols])] <- 0
  v2_all_sites$Cover_1 <- as.numeric(v2_all_sites$Cover_1)
  v2_all_sites$Cover_2 <- as.numeric(v2_all_sites$Cover_2)
  v2_all_sites$Cover_3 <- as.numeric(v2_all_sites$Cover_3)
  v2_all_sites$Cover_4 <- as.numeric(v2_all_sites$Cover_4)
  v2_all_sites$Cover_5 <- as.numeric(v2_all_sites$Cover_5)

  # Calculate average cover across all plots
  v2_sum <- v2_all_sites |> mutate(avg_cov = (Cover_1 + Cover_2 + Cover_3 + Cover_4 + Cover_5)/5) |>
    select(site_name, species, avg_cov)

  # Join with CoC
  vegcov_check <- left_join(v2_sum,
                            coc |> select(SPECIES, coc = ecoreg_82_COC, nativity, PLANTS = `PLANTS_Accepted Symbol`),
                            by = c("species" = "SPECIES"))

  # Check for species without CoCs, and manually add in. Where a genus is recorded, use the average C of species
  # in that genus that are native.
  spp_miss <- vegcov_check |> filter(is.na(coc)) |> select(-site_name, -avg_cov) |>
    unique() |> arrange(species)

  # Genus level scores (following NJNHP approach to filling in)
  AMELSPP <- coc |> filter(grepl("AMELANCHIER", SPECIES)) |> summarize(coc = mean(ecoreg_82_COC)) |> unique()
  VIOLA <- coc |> filter(grepl("VIOLA", SPECIES)) |> filter(nativity == "native") |>
    summarize(coc = mean(ecoreg_82_COC)) |> unique()

  # This isn't pretty, but doing it here, so I can check the values.
  spp_miss$coc[spp_miss$species == "ALNUS INCANA SPP. RUGOSA"] <- 3 # SSP in COC
  spp_miss$nativity[spp_miss$species == "ALNUS INCANA SPP. RUGOSA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "ALNUS INCANA SPP. RUGOSA"] <- "ALINR"

  spp_miss$coc[spp_miss$species == "AMELANCHIER"] <- as.numeric(AMELSPP)
  spp_miss$nativity[spp_miss$species == "AMELANCHIER"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "AMELANCHIER"] <- "AMELA"

  spp_miss$coc[spp_miss$species == "ANDROMEDA POLIFOLIA VAR. GLAUCOPHYLLA"] <- 7
  spp_miss$nativity[spp_miss$species == "ANDROMEDA POLIFOLIA VAR. GLAUCOPHYLLA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "ANDROMEDA POLIFOLIA VAR. GLAUCOPHYLLA"] <- "ANPOG"

  spp_miss$coc[spp_miss$species == "BETULA PAPYRIFERA"] <- 3 #B. pap var. pap.
  spp_miss$nativity[spp_miss$species == "BETULA PAPYRIFERA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "BETULA PAPYRIFERA"] <- "BEPA"

  spp_miss$coc[spp_miss$species == "CALOPOGON TUBEROSUS VAR. TUBEROSUS"] <- 7 #B. pap var. pap.
  spp_miss$nativity[spp_miss$species == "CALOPOGON TUBEROSUS VAR. TUBEROSUS"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "CALOPOGON TUBEROSUS VAR. TUBEROSUS"] <- "CATUT"

  spp_miss$coc[spp_miss$species == "CAREX ECHINATA VAR. ECHINATA"] <- 3 # C. echinata
  spp_miss$nativity[spp_miss$species == "CAREX ECHINATA VAR. ECHINATA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "CAREX ECHINATA VAR. ECHINATA"] <- "CAECE"

  spp_miss$coc[spp_miss$species == "CAREX LASIOCARPA SPP. AMERICANA"] <- 6 # C. lasiocarpa
  spp_miss$nativity[spp_miss$species == "CAREX LASIOCARPA SPP. AMERICANA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "CAREX LASIOCARPA SPP. AMERICANA"] <- "CALAA"

  spp_miss$coc[spp_miss$species == "CAREX MAGELLANICA SPP. IRRIGUA"] <- 7 # C. magellanica
  spp_miss$nativity[spp_miss$species == "CAREX MAGELLANICA SPP. IRRIGUA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "CAREX MAGELLANICA SPP. IRRIGUA"] <- "CAMAI2"

  spp_miss$coc[spp_miss$species == "CHAMAEPERICLYMENUM CANADENSE"] <- 5 #syn: Cornus canadensis
  spp_miss$nativity[spp_miss$species == "CHAMAEPERICLYMENUM CANADENSE"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "CHAMAEPERICLYMENUM CANADENSE"] <- "CHCA24"

  spp_miss$coc[spp_miss$species == "ERIOPHORUM ANGUSTIFOLIUM SPP. ANGUSTIFOLIUM"] <- 6 #syn: E. angustifolium
  spp_miss$nativity[spp_miss$species == "ERIOPHORUM ANGUSTIFOLIUM SPP. ANGUSTIFOLIUM"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "ERIOPHORUM ANGUSTIFOLIUM SPP. ANGUSTIFOLIUM"] <- "ERANA3"

  spp_miss$coc[spp_miss$species == "GAYLUSSACIA BIGELOVIANA"] <- 8 #syn: G. dumosa
  spp_miss$nativity[spp_miss$species == "GAYLUSSACIA BIGELOVIANA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "GAYLUSSACIA BIGELOVIANA"] <- "GABI7"

  spp_miss$coc[spp_miss$species == "KALMIA ANGUSTIFOLIA SPP. ANGUSTIFOLIA"] <- 4 #K. angustifolia
  spp_miss$nativity[spp_miss$species == "KALMIA ANGUSTIFOLIA SPP. ANGUSTIFOLIA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "KALMIA ANGUSTIFOLIA SPP. ANGUSTIFOLIA"] <- "KAAN"

  spp_miss$coc[spp_miss$species == "LYSIMACHIA BOREALIS"] <- 4 #syn: Trientalis borealis
  spp_miss$nativity[spp_miss$species == "LYSIMACHIA BOREALIS"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "LYSIMACHIA BOREALIS"] <- "TRIBO2" # USDA plants doesn't have LYSBOR code

  spp_miss$coc[spp_miss$species == "NUPHAR VARIEGATA"] <- 4 #Syn: N. lutea spp. variegata
  spp_miss$nativity[spp_miss$species == "NUPHAR VARIEGATA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "NUPHAR VARIEGATA"] <- "NUVA2"

  spp_miss$coc[spp_miss$species == "OSMUNDASTRUM CINNAMOMEUM"] <- 4 #syn: Osmunda cinnamomea
  spp_miss$nativity[spp_miss$species == "OSMUNDASTRUM CINNAMOMEUM"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "OSMUNDASTRUM CINNAMOMEUM"] <- "OSCI2"

  spp_miss$coc[spp_miss$species == "RHODODENDRON GROENLANDICUM"] <- 8 #syn: Ledum groenlandicum
  spp_miss$nativity[spp_miss$species == "RHODODENDRON GROENLANDICUM"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "RHODODENDRON GROENLANDICUM"] <- "RHGR3"

  spp_miss$coc[spp_miss$species == "RUBUS REPENS"] <- 5 #syn: Dalibarda repens
  spp_miss$nativity[spp_miss$species == "RUBUS REPENS"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "RUBUS REPENS"] <- "DARE"

  spp_miss$coc[spp_miss$species == "SARRACENIA PURPUREA SPP. PURPUREA"] <- 7 #S. purpurea
  spp_miss$nativity[spp_miss$species == "SARRACENIA PURPUREA SPP. PURPUREA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "SARRACENIA PURPUREA SPP. PURPUREA"] <- "SAPU4"

  spp_miss$coc[spp_miss$species == "SOLIDAGO ULIGINOSA VAR. PERACUTA"] <- 8 #S. purpurea
  spp_miss$nativity[spp_miss$species == "SOLIDAGO ULIGINOSA VAR. PERACUTA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "SOLIDAGO ULIGINOSA VAR. PERACUTA"] <- "SOULP"

  spp_miss$coc[spp_miss$species == "SPARGANIUM EMERSUM"] <- 3 #Not on current list, using previous version
  spp_miss$nativity[spp_miss$species == "SPARGANIUM EMERSUM"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "SPARGANIUM EMERSUM"] <- "SPEM2"

  spp_miss$coc[spp_miss$species == "THELYPTERIS PALUSTRIS VAR. PUBESCENS"] <- 3 #T.palustris
  spp_miss$nativity[spp_miss$species == "THELYPTERIS PALUSTRIS VAR. PUBESCENS"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "THELYPTERIS PALUSTRIS VAR. PUBESCENS"] <- "THPAP"

  spp_miss$coc[spp_miss$species == "TRICHOPHORUM CESPITOSUM SPP. CESPITOSUM"] <- 7 #T. cespitosum
  spp_miss$nativity[spp_miss$species == "TRICHOPHORUM CESPITOSUM SPP. CESPITOSUM"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "TRICHOPHORUM CESPITOSUM SPP. CESPITOSUM"] <- "TRCE3" # didn't have ssp code in USDA Plants

  spp_miss$coc[spp_miss$species == "UTRICULARIA VULGARIS SPP. MACRORHIZA"] <- 4 #U. machrorhiza
  spp_miss$nativity[spp_miss$species == "UTRICULARIA VULGARIS SPP. MACRORHIZA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "UTRICULARIA VULGARIS SPP. MACRORHIZA"] <- "UTVUM"

  spp_miss$coc[spp_miss$species == "VIOLA"] <- as.numeric(VIOLA)
  spp_miss$nativity[spp_miss$species == "VIOLA"] <- "native"
  spp_miss$PLANTS[spp_miss$species == "VIOLA"] <- "VIOLA"

  coc2 <- rbind(coc |> select(species = SPECIES, coc = ecoreg_82_COC, nativity, PLANTS = `PLANTS_Accepted Symbol`),
                spp_miss)

  vegcov_coc <- left_join(v2_sum, coc2, by = "species") |>
                mutate(stress_tol = ifelse(coc > 0 & coc <= 4, 1, 0))

  if(export_spp == TRUE){assign( "veg_spp_cov", vegcov_coc, envir = .GlobalEnv)}

  vegsum <- vegcov_coc |> group_by(site_name) |>
    summarize(mean_c = mean(coc),
              cov_tol = sum(avg_cov[stress_tol == 1]),
              cov_inv = sum(avg_cov[nativity == "non-native"]),
              .groups = 'drop')

  vmmi1 <- left_join(vegsum, v3_bryo |> select(SITE_ID, cov_bryo), by = c("site_name" = "SITE_ID")) |>
    mutate(mean_c_adj = ((mean_c - 3.015)/(7.346 - 3.015)) * 10,
           cov_tol_adj = ((((cov_tol-0.386)/(136.645 - 0.386)) * 10) - 10) * -1, # reverse the scale
           cov_inv_adj = (((cov_inv/38.45) * 10) - 10) * -1,
           cov_bryo_adj = (cov_bryo/98.48) * 10,
           vmmi1 = mean_c_adj + cov_tol_adj + cov_inv_adj + cov_bryo_adj,
           vmmi = ((vmmi1 - 0.389)/(40 - 0.389)) * 100,
           vmmi_rank = ifelse(vmmi > 65.22746, "Good", ifelse(vmmi < 52.785, "Poor", "Fair"))
    ) |> arrange(site_name)

  # Add local name to ACAD sites
  ACAD_sites <-  data.frame(site_name =
                              c("NWC21-ME-HP301", "NWC21-ME-HP302", "NWC21-ME-HP303", "NWC21-ME-HP304", "NWC21-ME-HP305",
                                "NWC21-ME-HP306", "NWC21-ME-HP307", "NWC21-ME-HP308", "NWC21-ME-HP309", "NWC21-ME-HP310"),
                            LOCAL_ID =
                              c("DUCK", "WMTN", "BIGH", "GILM", "LIHU",
                                "NEMI", "GRME", "HEBR", "HODG", "FRAZ"))


  vmmi <- dplyr::left_join(ACAD_sites, vmmi1, by = c("site_name"))
  return(vmmi)

  }


