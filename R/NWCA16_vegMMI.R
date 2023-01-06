#' @title NWCA16_vegMMI: calculate Vegetation Multi-metric Index
#'
#' @description This function calculates the 4 metrics used to calculate the vegetation multi-metric index (MMI) developed by Miller et al. (2016). Note that to calculate the MMI, the species data must be joined with the Coefficient of Conservatism ranking. This is only implemented for EcoRegion 82 and downloads the CoCs from the Maine Natural Areas Program's website: https://www.maine.gov/dacf/mnap/features/coc.htm. This function is only implemented for ACAD sites and will break if the url hosting the CoC rankings changes.
#'
#'
#' @importFrom dplyr filter mutate select
#'
#' @return NWCA16 data files as data frames in specified environment
#'
#' @examples
#' \dontrun{
#'
#' #+++++ ADD EXAMPLES +++++
#' # Import ACAD only data
#' NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
#'
#' }
#'
#' @export

NWCA16_vegMMI<- function(){

  # Check that suggested package required for this function are installed
  if(!requireNamespace("httr", quietly = TRUE)){
    stop("Package 'httr' needed for this function to work. Please install it.", call. = FALSE)
  }

  if(!requireNamespace("readxl", quietly = TRUE)){
    stop("Package 'readxl' needed for this function to work. Please install it.", call. = FALSE)
  }


  # Error handling
  stopifnot(class(new_env) == 'logical')
  stopifnot(class(ACAD_only) == 'logical')

  # Extract veg data
  env = if(exists("NWCA16")){NWCA16} else {.GlobalEnv}

   tryCatch(vegcov <- get("veg_data", envir = env) |>
               select(UID, SITE_ID, LOCAL_ID, YEAR, VISIT_NO, DATE_COL, PLOT,
                      SPECIES, SPECIES_NAME_ID, COVER, HEIGHT, NE, SW),
           error = function(e){stop("The veg_data data frame was not found. Please import the data.")})

  # Importing COC table from MNAP website
   tryCatch(
   {url <- "https://www.maine.gov/dacf/mnap/features/Maine_CoC.xlsx"
    httr::GET(url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    coc <- readxl::read_excel(tf) |>
      mutate(SPECIES = toupper(`Scientific Name`)) |>
      select(acronym, "PLANTS_Accepted Symbol", "SPECIES",
                                           nativity, "physiognomy", "duration",
                                           ecoreg_82_COC, "coefficient of wetness")},
    error = function(e){stop(paste0("Unable to download data from MNAP link: ", url))})

  # Join COC to vegcov
  vegcovc <- left_join(vegcov, coc |> select("SPECIES", "ecoreg_82_COC", "nativity"),
                       by = "SPECIES")

  vegcovc$ecoreg_82_COC <- as.numeric(vegcovc$ecoreg_82_COC)

  # Check for species without CoCs, and manually add in. Where a genus is recorded, use the average C of species
  # in that genus that are native.
  spp_miss <- vegcovc |> filter(is.na(ecoreg_82_COC)) |>
    select(SPECIES, ecoreg_82_COC) |> unique()

  spp_miss$ecoreg_82_COC <- as.numeric(spp_miss$ecoreg_82_COC)

  # Genus level scores
  AMELSPP <- coc |> filter(grepl("AMELANCHIER", SPECIES)) |> summarize(coc = mean(ecoreg_82_COC))
  VIOLA <- coc |> filter(grepl("VIOLA", SPECIES)) |> filter(nativity == "native") |>
    summarize(coc = mean(ecoreg_82_COC))

  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "AMELANCHIER"] <- AMELSPP
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "VIOLA"] <- VIOLA
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "OCLEMENA xBLAKEI"] <- 6 # average of OCLACU (4) and OCLNEM (8)
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "BETULA PAPYRIFERA"] <- 3 # On CoC list as BETPAP var. PAP
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "VIBURNUM NUDUM"] <- 5 # On CoC list as VIBNUD var. CAS
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "ALNUS INCANA"] <- 3 # On CoC list as ALNINC var. RUG
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "RUBUS IDAEUS"] <- 2 # CoC list includes 2 vars, but both are 2.
  spp_miss$ecoreg_82_COC[spp_miss$SPECIES == "SPARGANIUM EMERSUM"] <- 3 # Missing from CoC list; taken from prev. version of list.

  vegcovc2 <- left_join(vegcovc, spp_miss, by = c("SPECIES"))
   # ENDED HERE. NEED TO CLEAN UP SO FIXED COC REPLACES NAS

  }
