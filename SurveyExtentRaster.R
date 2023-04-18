# Script to generate bland raster for U.S. West Coast Continental Margin
# and clip to NMFS West Coast Groundfish Bottom Trawl Survey extent
# Workflow adopted from https://rspatial.org/raster/pkg/3-objects.html, and
# https://geobgu.xyz/r-2020/combining-rasters-and-vector-layers.html, and
# https://datacarpentry.org/semester-biology/materials/spatial-data-cropping-R/

# Load libraries
library(sf)
library(raster)
library(stars)

# Import shapefiles
gridBTSutm <- sf::st_read("/Users/curt.whitmire/Documents/github/_data/GIS/WCGBTS_Grid_v2008_utm/WCGBTS_Grid_v2008_utm.shp")
# Rview crs of imported shapefile
st_crs(gridBTSutm)
gridBTSalb <- sf::st_read("/Users/curt.whitmire/Documents/github/_data/GIS/WCGBTS_Grid_v2008_alb/WCGBTS_Grid_v2008_alb.shp")
# Review crs of imported shapefile
st_crs(gridBTSalb)

# Define the raster variables, including crs, resolution, abd default cell values
projUTM <- "+proj=utm +zone=10 +datum=WGS84" # WGS84 UTM Zone 10
projALB <- "+proj=aea +lat_1=32 +lat_2=48 +lat_0=40 +lon_0=-126 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" # Custom NAD83 Albers
hres <- 1000
rval <- 1

# Create RasterLayer object with the extent parameters from shapefile
x <- raster::raster(resolution=hres, # set horizontal resolution
                    xmn=st_bbox(gridBTSalb)[1], 
                    xmx=st_bbox(gridBTSalb)[3], 
                    ymn=st_bbox(gridBTSalb)[2], 
                    ymx=st_bbox(gridBTSalb)[4], 
                    crs=projALB, # set crs
                    vals=rval) # set default cell value
# Describe RasterLayer object
x

# Convert raster to stars object
r_alb <- stars::st_as_stars(x)
r_alb

# Crop the raster by the extent shapefile
r_alb <- r_alb[gridBTSalb]
r_alb

# Project cropped raster to UTM
r_utm = st_warp(r_alb, crs = st_crs(gridBTSutm))
r_utm

# Plot the cropped raster
plot(r_alb, reset = FALSE)
plot(st_geometry(gridBTSalb), add = TRUE)

# Save output rasters
stars::write_stars(r_alb, paste0("WCGBTS_raster_", hres, "m_alb.tif"))
stars::write_stars(r_utm, paste0("WCGBTS_raster_", hres, "m_utm.tif"))
