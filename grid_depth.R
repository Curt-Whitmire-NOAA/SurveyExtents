# Script to process grid base to summarize water depth info
# based on workflow "Extracting raster values into polygon attributes using R"
# Author: Luis D. Verde Arregoitia
# https://luisdva.github.io/rstats/GIS-with-R/

library(rstudioapi)
library(sf) # Simple Features for R
library(rnaturalearth) # World Map Data from Natural Earth
library(here) # A Simpler Way to Find Your Files
library(stars) # Spatiotemporal Arrays, Raster and Vector Data Cubes
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggnewscale) # Multiple Fill and Color Scales in 'ggplot2'
library(scico) # Colour Palettes Based on the Scientific Colour-Maps
library(geobgu) # install from GitHub ("michaeldorman/geobgu")
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with 'ggplot2'

# Set working directories
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

outPath <- "/Users/curt.whitmire/Documents/github/_data/GIS" # Data output location

# load shapefiles
gridNEP <- st_read("/Users/curt.whitmire/Documents/github/_data/GIS/Grid_Base_jCentroidInfo_dd.shp")

# load raster
bathyNCEI <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/NCEI_GridExtract/NCEI_GridExtract_20221205.tif")
bathyNCEIN <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/NCEI_GridExtract/NCEI_GridExtract_North.tiff")
bathyNCEIS <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/NCEI_GridExtract/NCEI_GridExtract_South.tiff")
bathyETOPO <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/ETOPO_v2022/ETOPO_v2022_sub.tiff")
bathyGEBCO <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/GEBCO_01_Dec_2022_358306d053ed/gebco_2022_n51.0_s30.0_w-130.0_e-116.0.tif")

# crop the raster (NCEI)
bathyNCEI_crop <- st_crop(bathyNCEI, gridNEP)
plot(bathyNCEI_crop)

# crop the raster (NCEI-North); breaking up raster into smaller pieces
bathyNCEIN_crop <- st_crop(bathyNCEIN, gridNEP)
plot(bathyNCEIN_crop)

# crop the raster (NCEI-South); breaking up raster into smaller pieces
bathyNCEIS_crop <- st_crop(bathyNCEIS, gridNEP)
plot(bathyNCEIS_crop)

# Extract the underlying depth values (NCEI) for each cell in the polygon grid
gridNEP <-
  gridNEP %>% mutate(
    zAvg_m_CRM = -1 * geobgu::raster_extract(bathyNCEI, gridNEP, fun = mean, na.rm = TRUE),
    zMin_m_CRM = -1 * geobgu::raster_extract(bathyNCEI, gridNEP, fun = max, na.rm = TRUE),
    zMax_m_CRM = -1 * geobgu::raster_extract(bathyNCEI, gridNEP, fun = min, na.rm = TRUE)
  ) %>% mutate(
    zAvg_f_CRM = zAvg_m_CRM * 0.546807,
    zMin_f_CRM = zMin_m_CRM * 0.546807,
    zMax_f_CRM = zMax_m_CRM * 0.546807
  )

# crop the raster (ETOPO)
bathyETOPO_crop <- st_crop(bathyETOPO, gridNEP)
plot(bathyETOPO_crop)

# Extract the underlying depth values (ETOPO) for each cell in the polygon grid
gridNEP <-
  gridNEP %>% mutate(
    zAvg_m_ETO = -1 * geobgu::raster_extract(bathyETOPO_crop, gridNEP, fun = mean, na.rm = TRUE),
    zMin_m_ETO = -1 * geobgu::raster_extract(bathyETOPO_crop, gridNEP, fun = max, na.rm = TRUE),
    zMax_m_ETO = -1 * geobgu::raster_extract(bathyETOPO_crop, gridNEP, fun = min, na.rm = TRUE)
  ) %>% mutate(
    zAvg_f_ETO = zAvg_m_ETO * 0.546807,
    zMin_f_ETO = zMin_m_ETO * 0.546807,
    zMax_f_ETO = zMax_m_ETO * 0.546807
  )

# crop the raster (GEBCO)
bathyGEBCO_crop <- st_crop(bathyGEBCO, gridNEP)
plot(bathyGEBCO_crop)

# Extract the underlying depth values (GEBCO) for each cell in the polygon grid
gridNEP <-
  gridNEP %>% mutate(
    zAvg_m_GEB = -1 * geobgu::raster_extract(bathyGEBCO_crop, gridNEP, fun = mean, na.rm = TRUE),
    zMin_m_GEB = -1 * geobgu::raster_extract(bathyGEBCO_crop, gridNEP, fun = max, na.rm = TRUE),
    zMax_m_GEB = -1 * geobgu::raster_extract(bathyGEBCO_crop, gridNEP, fun = min, na.rm = TRUE)
  ) %>% mutate(
    zAvg_f_GEB = zAvg_m_GEB * 0.546807,
    zMin_f_GEB = zMin_m_GEB * 0.546807,
    zMax_f_GEB = zMax_m_GEB * 0.546807
  )

# Add field strata assignment based on each bathy dataset min/max values
# NEW Method of calculating strata values; need to verify this is correct method
gridNEP <-
  gridNEP %>% mutate(stratETOPO = case_when(
    !(is.na(StnCode)) & zAvg_f_ETO >= 30 & zAvg_f_ETO <100 ~ "30-100",
    !(is.na(StnCode)) & zAvg_f_ETO >= 100 & zAvg_f_ETO <300 ~ "100-300",
    !(is.na(StnCode)) & zAvg_f_ETO >= 300 & zAvg_f_ETO <700 ~ "300-700")
  ) %>% mutate(stratGEBCO = case_when(
    !(is.na(StnCode)) & zAvg_f_GEB >= 30 & zAvg_f_GEB <100 ~ "30-100",
    !(is.na(StnCode)) & zAvg_f_GEB >= 100 & zAvg_f_GEB <300 ~ "100-300",
    !(is.na(StnCode)) & zAvg_f_GEB >= 300 & zAvg_f_GEB <700 ~ "300-700")
  )

# OLD Method of calculating strata values
gridNEP <-
  gridNEP %>% mutate(stratETOPO = case_when(
    !(is.na(StnCode)) & zMin_f_ETO >= 30 & zMax_f_ETO <100 ~ "30-100",
    !(is.na(StnCode)) & zMin_f_ETO >= 100 & zMax_f_ETO <300 ~ "100-300",
    !(is.na(StnCode)) & zMin_f_ETO >= 300 & zMax_f_ETO <700 ~ "300-700")
  ) %>% mutate(stratGEBCO = case_when(
    !(is.na(StnCode)) & zMin_f_GEB >= 30 & zMax_f_GEB <100 ~ "30-100",
    !(is.na(StnCode)) & zMin_f_GEB >= 100 & zMax_f_GEB <300 ~ "100-300",
    !(is.na(StnCode)) & zMin_f_GEB >= 300 & zMax_f_GEB <700 ~ "300-700")
  )

# Show table summary
summary(gridNEP)

# Check for any missing strata values for WCGBTS cells
grid_sub <- 
  gridNEP %>% 
  select(StnCode,StratumZ,stratETOPO,contains("f_ETO"),stratGEBCO,contains("f_GEB")) %>% 
  filter(!(is.na(StnCode)) & (is.na(stratETOPO) | is.na(stratGEBCO))) %>% 
  arrange(zMin_f_ETO,zMin_f_GEB)
  

# Save the output grid in shapefile and RDA formats
save(gridNEP, bathyETOPO_crop, bathyGEBCO_crop, file = paste(outPath, "grid_NEPacific_wBathy.rda", sep = "/"), compress = "xz")
st_write(gridNEP, paste0(outPath, "/", "grid_NEPacific_wBathy.shp"), append=FALSE, delete_layer = TRUE)

# Temporary work
gridNEP <- 
  gridNEP %>% rename(
    stratETOPO = strataETOPO,
    stratGEBCO = strataGEBCO
  )

