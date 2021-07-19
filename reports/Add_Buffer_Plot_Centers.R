#--------------------
# Code to generate new buffer plot locations for 2021
#--------------------
library(tidyverse)
library(sf)

AAcenters <- read.csv("D:/NETN/GIS/ACAD/Wetlands/Sample_Locations/2021_NWCA_AA_and_Buffer_Plot_Centers_UTM.csv")
colnames(AAcenters) <- c("Site", "Type", "X", "Y", "Priority", "GRTS", "Panel", "Notes")

AAsf <- st_as_sf(AAcenters, coords = c("X", "Y"), crs = 26919)
AA_cent <- st_coordinates(st_geometry(AAsf), crs = 26919)
AA_N1 <- st_coordinates(st_geometry(AAsf) + c(0, 70), crs = 26919)
AA_N2 <- st_coordinates(st_geometry(AAsf) + c(0, 100), crs = 26919)
AA_N3 <- st_coordinates(st_geometry(AAsf) + c(0, 135), crs = 26919)

AA_E1 <- st_coordinates(st_geometry(AAsf) + c(70, 0), crs = 26919)
AA_E2 <- st_coordinates(st_geometry(AAsf) + c(100, 0), crs = 26919)
AA_E3 <- st_coordinates(st_geometry(AAsf) + c(135, 0), crs = 26919)

AA_S1 <- st_coordinates(st_geometry(AAsf) + c(0, -70), crs = 26919)
AA_S2 <- st_coordinates(st_geometry(AAsf) + c(0, -100), crs = 26919)
AA_S3 <- st_coordinates(st_geometry(AAsf) + c(0, -135), crs = 26919)

AA_W1 <- st_coordinates(st_geometry(AAsf) + c(-70, 0), crs = 26919)
AA_W2 <- st_coordinates(st_geometry(AAsf) + c(-100, 0), crs = 26919)
AA_W3 <- st_coordinates(st_geometry(AAsf) + c(-135, 0), crs = 26919)


Local_Name = c("DUCK", "WMTN", "BIGH", "GILM", "LIHU", "NEMI", "GRME", "HEBR", "HODG", "FRAZ")

AA_comb <- data.frame(site = rep(unique(AAcenters$Site), times = 13),
                      code = rep(Local_Name, times = 13),
                      buff_plot = rep(c("AA", "N1", "N2", "N3", "E1", "E2", "E3",
                                        "S1", "S2", "S3", "W1", "W2", "W3"), each = 10),
                      X = c(AA_cent[,1],
                            AA_N1[,1], AA_N2[,1], AA_N3[,1], AA_E1[,1], AA_E2[,1], AA_E3[,1],
                            AA_S1[,1], AA_S2[,1], AA_S3[,1], AA_W1[,1], AA_W2[,1], AA_W3[,1]),
                      Y = c(AA_cent[,2],
                            AA_N1[,2], AA_N2[,2], AA_N3[,2], AA_E1[,2], AA_E2[,2], AA_E3[,2],
                            AA_S1[,2], AA_S2[,2], AA_S3[,2], AA_W1[,2], AA_W2[,2], AA_W3[,2])) %>%
                      mutate(xcoord = X, ycoord = Y,
                             Label = paste(code, buff_plot, sep = "_"))


AA_comb_sf <- st_as_sf(AA_comb, coords = c("X", "Y"), crs = 26919)
AA_comb_dd <- st_transform(AA_comb_sf, crs = 4269)

AA_cent_dd <- AA_comb_dd %>% filter(buff_plot == "AA")

AA_cent_dd_df <- data.frame(site = AA_cent_dd$site,
                            code = AA_cent_dd$code,
                            lat = st_coordinates(AA_cent_dd)[,2],
                            lon = st_coordinates(AA_cent_dd)[,1]
                            )
write.csv(AA_cent_dd_df, "D:/NETN/Monitoring_Projects/Freshwater_Wetland/2021/Logistics/AA_Centers_DD.csv",
          row.names = F)

#st_crs(4269)
st_write(AA_comb_sf, "D:/NETN/GIS/ACAD/Wetlands/Sample_Locations/2021_NWCA_AA_and_Buffer_Centers_UTM.shp")#, append = T)
st_write(AA_comb_dd, "D:/NETN/GIS/ACAD/Wetlands/Sample_Locations/2021_NWCA_AA_and_Buffer_Centers_dd.shp")#, append = T)



