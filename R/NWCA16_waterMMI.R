#' @title NWCA16_waterMMI: calculate Water Chemistry Multi-metric Index
#'
#' @description This function calculates the 3 metrics used to calculate the water chemistry multi-metric index (MMI) developed by Miller et al. (2016). This function is only implemented for ACAD sites.
#'
#' @importFrom dplyr arrange filter mutate select
#'
#' @return Data frame with water MMI metrics and ratings
#'
#' @examples
#' \dontrun{
#'
#' # Import ACAD only data and export
#' NWCA16_import(path = "../data/NWCA16", zip_name = "NWCA2016_data.zip")
#' wmmi <- NWCA16_wegMMI()
#'
#' }
#'
#' @export

NWCA16_waterMMI <- function(){

  # Extract soil data
  env = if(exists("NWCA16")){NWCA16} else {.GlobalEnv}

  tryCatch(
    {wchem1 <- get("water_chem", envir = env) |>
      dplyr::select(UID, SITE_ID, VISIT_NO, DATE_COL,
                    pH = PH_RESULT, COND_RESULT, PTL_RESULT) |>
      dplyr::mutate(logCond = log10(COND_RESULT), logTP = log10(PTL_RESULT))},
    error = function(e){stop("The water_chem data frame was not found. Please import the data.")})

  # filter on ACAD sites and add local name
  ACAD_sites <-  data.frame(SITE_ID =
                              c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                                "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310"),
                            LOCAL_ID =
                              c("DUCK", "WMTN", "BIGH", "GILM", "LIHU",
                                "NEMI", "GRME", "HEBR", "HODG", "FRAZ"))


  wchem2 <- wchem1 |> dplyr::filter(SITE_ID %in% ACAD_sites$SITE_ID)
  wchem <- dplyr::left_join(wchem2, ACAD_sites, by = "SITE_ID")

  # Check that there's only 1 record per site
  if(length(unique(wchem$SITE_ID)) < nrow(wchem)){
    warning("There are more than 10 water chemistry records in the dataset. Check for duplicates or sites outside of ACAD.")}

  # Join datasets and calc. SMMI based on Miller et al. 2017., which requires adjusting
  # the floor and ceiling for each metric

  wmmi <- wchem |>
    mutate(pH_adj = ((((pH - 4.2)/(7.748 - 4.2)) * 10) - 10) * -1, # reverse the scale
           logCond_adj = ((((logCond - 1.26533)/(2.650351 - 1.26533)) * 10) - 10) * -1,
           logTP_adj = ((((logTP - 0.679588)/(3.09319 - 0.679588)) * 10) - 10) * -1,
           wmmi1 = pH_adj + logCond_adj + logTP_adj,
           wmmi = ((wmmi1 - 0.956494)/(26.65048 - 0.956494)) * 100,
           wmmi_rank = ifelse(wmmi > 68.00122, "Good", ifelse(wmmi < 55.86188, "Poor", "Fair"))
    ) |> arrange(SITE_ID)

  wmmi
  return(wmmi)

  }
