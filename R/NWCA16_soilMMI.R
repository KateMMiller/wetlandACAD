#' @title NWCA16_soilMMI: calculate Soil Multi-metric Index
#'
#' @description This function calculates the 4 metrics used to calculate the soil multi-metric index (MMI) developed by Miller et al. (2016) using the top horizon from the soil chemistry horizon data (not the standard core) to be consistent with previous analyses. This function is only implemented for ACAD sites.
#'
#' @importFrom dplyr arrange filter mutate select
#'
#' @return Data frame with soil MMI metrics and ratings
#'
#' @examples
#' \dontrun{
#'
#' # Import ACAD only data and export
#' NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
#' smmi <- NWCA16_soilMMI()
#'
#' }
#'
#' @export

NWCA16_soilMMI <- function(){

  # Extract soil data
  env = if(exists("NWCA16")){NWCA16} else {.GlobalEnv}

  tryCatch(
    {soilchem1 <- get("soil_chem_hor", envir = env) |>
                 dplyr::select(UID, SITE_ID, VISIT_NO, DATE_COL, HORIZON,
                               PctTC = C_TOT_ELEM_ANLYS, Co = CO_TOT_ELEM_ANLYS,
                               P = P_NH4OXA_EXTR, pH = PH_CACL2_SOLN) |>
                 dplyr::filter(HORIZON == 1)},
     error = function(e){stop("The soil_chem_hor data frame was not found. Please import the data.")})

  # filter on ACAD sites and add local name
  ACAD_sites <-  data.frame(SITE_ID =
                              c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                                "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310"),
                            LOCAL_ID =
                              c("DUCK", "WMTN", "BIGH", "GILM", "LIHU",
                                "NEMI", "GRME", "HEBR", "HODG", "FRAZ"))


  soilchem2 <- soilchem1 |> dplyr::filter(SITE_ID %in% ACAD_sites$SITE_ID)
  soilchem <- dplyr::left_join(soilchem2, ACAD_sites, by = "SITE_ID")

  # Check that there's only 1 record per site
  if(length(unique(soilchem$SITE_ID)) < nrow(soilchem)){
    warning("There are more than 10 soil chemistry records in the dataset. Check for duplicates or sites outside of ACAD.")}

  # Join datasets and calc. SMMI based on Miller et al. 2017., which requires adjusting
  # the floor and ceiling for each metric

  smmi <- soilchem |>
    mutate(PctTC_adj = ((PctTC - 1.29)/(55.9 - 1.29)) * 10,
           Co_adj = ((((Co - 0.282)/(11.884 - 0.282)) * 10) - 10) * -1, # reverse the scale
           P_adj = (((P/786.35) * 10) - 10) * -1,
           pH_adj = ((((pH - 2.96)/(6.79 - 2.96)) * 10) - 10) * -1,
           smmi1 = PctTC_adj + Co_adj + P_adj + pH_adj,
           smmi = ((smmi1 - 4.215839)/(39.8606 - 4.215839)) * 100,
           smmi_rank = ifelse(smmi > 71.38558, "Good", ifelse(smmi < 46.89643, "Poor", "Fair"))
    ) |> arrange(SITE_ID)

  return(smmi)

  }
