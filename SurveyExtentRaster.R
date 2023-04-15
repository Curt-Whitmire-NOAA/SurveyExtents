# Script to generate bland raster for U.S. West Coast Continental Margin
# and clip to NMFS West Coast Groundfish Bottom Trawl Survey extent
# Workflow adopted from https://rspatial.org/raster/pkg/3-objects.html, and
# https://geobgu.xyz/r-2020/combining-rasters-and-vector-layers.html

library(sf)
library(raster)
library(stars)
# library(gridExtra)

# load shapefiles
# gridBTSdd <- st_read("/Users/curt.whitmire/Documents/github/_data/GIS/WCGBTS_Grid_v2008_dd/WCGBTS_Grid_v2008_dd.shp")
# st_crs(gridBTSdd)
gridBTSutm <- st_read("/Users/curt.whitmire/Documents/github/_data/GIS/WCGBTS_Grid_v2008_utm/WCGBTS_Grid_v2008_utm.shp")
st_crs(gridBTSutm)
# gridBTSalb <- st_read("/Users/curt.whitmire/Documents/github/_data/GIS/WCGBTS_Grid_v2008_alb/WCGBTS_Grid_v2008_alb.shp")
# st_crs(gridBTSalb)

# Create RasterLayer with the extent parameters from shapefile
# x <- raster(ncol=817, nrow=1822, xmn=st_bbox(gridBTSalb)[1], xmx=st_bbox(gridBTSalb)[3], ymn=st_bbox(gridBTSalb)[2], ymx=st_bbox(gridBTSalb)[4], vals=1)
x <- raster(ncol=761, nrow=1829, xmn=st_bbox(gridBTSutm)[1], xmx=st_bbox(gridBTSutm)[3], ymn=st_bbox(gridBTSutm)[2], ymx=st_bbox(gridBTSutm)[4], vals=1)
# x <- raster(ncol=105, nrow=198, xmn=st_bbox(gridBTSdd)[1], xmx=st_bbox(gridBTSdd)[3], ymn=st_bbox(gridBTSdd)[2], ymx=st_bbox(gridBTSdd)[4], vals=1)

# set the coordinate reference system (CRS) (define the projection)
projection(x) <- "+proj=utm +zone=10 +datum=WGS84"

# Describe RasterLayer
x
res(x)

# change resolution
res(x) <- 1000
res(x)
ncol(x)
nrow(x)

# Convert raster to stars object
r <- st_as_stars(x)
r
# Crop the raster by the shapefile
r_crop <- r[gridBTSutm, crop=TRUE]
# r_crop <- st_crop(r, gridBTSutm)
dim(r)
dim(r_crop)

# Plot the rasters, side by side
par(mfrow = c(1, 2))
raster::plot(r)
raster::plot(r_crop)
par(mfrow = c(1,1))

# Save output
write_stars(r_crop, "WCGBTS_raster.tif")
