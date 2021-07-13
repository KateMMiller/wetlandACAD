library(tidyverse)
filepath <- "D:/NETN/Monitoring_Projects/Freshwater_Wetland/EPA_NWCA/2016_Data/2021_release/ACAD_only"
filenames <- list.files(filepath)
list.files(filepath)

# Import NWCA data and drop columns with all NA
import_csv <- function(filename){
  df <- read.csv(paste0(filepath, "/", filename))
  df <- df[,colSums(is.na(df)) < nrow(df)]
  return(df)
}

# Site characterization data
AAchar <- import_csv(filenames[1])[, c("SITE_ID", "Local_Name" ,"VALXSITE", "AA_AREA", "AA_CENTER",
                                       "AA_LAT", "AA_LON", "AA_LAYOUT", "AA_POINT",
                                       "HGM_CLASS", "DEPRESSION", "FLATS", "RIVERINE", "SLOPE")]
AAchar$data_type <- "AA_Char"

AA_DUCK <- AAchar %>% filter(Local_Name == "DUCK")
AA_DUCK <- AA_DUCK[,colSums(is.na(AA_DUCK)) < nrow(AA_DUCK)]

cat(names(AA_DUCK), sep = '\n')

print(AA_DUCK[1,], sep = '\n')

AAchar_long <- AAchar %>% pivot_longer(cols = c( AA_LAYOUT, AA_POINT, HGM_CLASS, DEPRESSION, FLATS, RIVERINE, SLOPE),
                                       names_to = "Metric",
                                       values_to = "Class") %>% na.omit(.)

AAhydro_source <- import_csv(filenames[2])[, c("SITE_ID", "Local_Name", "OUTFLOWS", "INFLOW_PRESENT", "PRECIP_PRESENT",
                                               "GRDWATER_PRESENT", "INFLOW_RANK", "PRECIP_RANK", "GRDWATER_RANK")]
AAhydro_source$data_type <- "AA_HydroSources"

AAHydroInd <- import_csv(filenames[4])
AAHydroInd$data_type <- "AA_HydroInd"

waterchar <- import_csv(filenames[19])
waterchar$data_type <- "Water_Char"




#----- Files not included in summary
   # filenames[3] # AA Hydrology Stressors: No stressors recorded in sentinel AAs
   # filenames[5] # is Buffer Natural Cover, which was dropped from 2021 NWCA
   # filenames[7] # Floras/site; not
   # filenames[9] # Microcystin #All ACAD sites < MDL
   # filenames[11] # Site Info already included in AAchar
   # filenames[12:15] # Soil data, which we're skipping in 2021. Will go back and calculate soil MMI when time
   # filenames[17] # Vegplot GPS location: already have these programmed (except FRAZER)
   # filenames[20] # water chemistry. Go back and calculated water MMI when time

#--- Data to reshape
buff_stress <- import_csv(filenames[6]) # Row for each plot with a stressor, including a row for each site with none
# Need to reshape

# Vegetation Plot Data
ground_char <- import_csv(filenames[8]) # 1 row per veg plot/site
vegcov <- import_csv(filenames[10]) # Already analyzed with NWCA16_ACAD_species_lists.RMD
trees <- import_csv(filenames[16]) # funky design with dead and live trees included. Need to reshape/summarize
stratacov <- import_csv(filenames[18]) # 1 row per veg plot/site


filenames
