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
bathy <- read_stars("/Users/curt.whitmire/Documents/github/_data/GIS/ETOPO_v2022_sub.tiff")

# crop the raster
bathyNEP <- stars::st_crop(bathy, gridNEP)
plot(bathyNEP)

# Extract the underlying depth values for each cell in the polygon grid
gridNEP <-
  gridNEP %>% mutate(
    zAvg = geobgu::raster_extract(bathyNEP, gridNEP, fun = mean, na.rm = TRUE),
    zMax = geobgu::raster_extract(bathyNEP, gridNEP, fun = max, na.rm = TRUE),
    zMin = geobgu::raster_extract(bathyNEP, gridNEP, fun = min, na.rm = TRUE),
  )

gridNEP %>%
  st_set_geometry(NULL) %>%
  knitr::kable()

summary(gridNEP)

# Save the output grid in shapefile and RDA formats
save(gridNEP, bathyNEP, file = paste(outPath, "grid_NEPacific_wBathy.rda", sep = "/"), compress = "xz")
st_write(gridNEP, paste0(outPath, "/", "grid_NEPacific_wBathy.shp"))
