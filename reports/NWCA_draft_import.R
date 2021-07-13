#-------------------------
# Code to import the 2016 EPA NWCA data currently in DRAFT Form.
#-------------------------
library(tidyverse)
library(readxl)

# Set up site list for ACAD
ACAD_sites <- c("NWCA16-R301", "NWCA16-R302", "NWCA16-R303", "NWCA16-R304", "NWCA16-R305",
                "NWCA16-R306", "NWCA16-R307", "NWCA16-R308", "NWCA16-R309", "NWCA16-R310")
ACAD_names <- c("DUCK", "WMTN", "BIGH", "GILM", "LIHU", "NEMI", "GRME", "HEBR", "HODG", "FRAZ")
ACAD_tbl <- data.frame(SITE_ID = ACAD_sites, Local_Name = ACAD_names)

# Function that reads in full data and exports ACAD only sites
compile_ACAD_data <- function(path, filename){
  path <- if(substr(path, nchar(path), nchar(path))!="/"){
    paste0(path, "/")
  } else(paste0(path))
  df <- read_xlsx(path = paste0(path, filename), sheet = "Data")
  df2 <- df %>% filter(SITE_ID %in% ACAD_sites) %>%
                full_join(., ACAD_tbl, by = "SITE_ID") %>%
                select(UID, SITE_ID, Local_Name, everything())
  write.csv(df2, paste0(path, "ACAD_only/", substr(filename, 1, nchar(filename)-5), "_ACAD.csv"), row.names = FALSE)
}

# iterate through NWCA datasets to pull out only ACAD sites
filepath <- "D:/NETN/Monitoring_Projects/Freshwater_Wetland/EPA_NWCA/2016_Data/2021_release/"
filenames <- list.files(filepath)[grep("DRAFT", list.files(filepath))]#[1:20]

lapply(seq_along(filenames), function(x){compile_ACAD_data(path = filepath, filename = filenames[x])})
