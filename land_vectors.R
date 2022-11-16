# Script to process vector coastline from NaturalEarth and GEBCO
# based on workflow in {ggOceanMaps} User Manual
# Author: Mikko Vihtakari (Institute of Marine Research)
# https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html

citation("ggOceanMaps")
#> 
#> To cite package 'ggOceanMaps' in publications use:
#> 
#>   Vihtakari M (2022). _ggOceanMaps: Plot Data on Oceanographic Maps
#>   using 'ggplot2'_. R package version 1.3.4,
#>   <https://mikkovihtakari.github.io/ggOceanMaps/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {ggOceanMaps: Plot Data on Oceanographic Maps using 'ggplot2'},
#>     author = {Mikko Vihtakari},
#>     year = {2022},
#>     note = {R package version 1.3.4},
#>     url = {https://mikkovihtakari.github.io/ggOceanMaps/},
#>   }

# load requisite libraries
library(rstudioapi)
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(sp)
library(ggOceanMapsData)
library(ggOceanMaps)

# # Set working directory and relevant paths
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

etopoPath <- "/Users/curt.whitmire/Documents/github/_data/GIS" # ETOPO1 grd file is located.
NEDPath <- "/Users/curt.whitmire/Documents/github/_data/GIS" # Natural Earth Data location
outPath <- "/Users/curt.whitmire/Documents/github/_data/GIS" # Data output location

# Process vector land file
world <- rgdal::readOGR(paste(NEDPath, "ne_10m_land/ne_10m_land.shp", sep = "/"))
islands <- rgdal::readOGR(paste(NEDPath, "ne_10m_minor_islands/ne_10m_minor_islands.shp", sep = "/"))
world <- rbind(world, islands)

# Set map projection and limits
lims = c(-130, -115, 30, 60) # for WGS84
# projection <- "EPSG:4326" # WGS84
projection <- "EPSG:32610" # WGS84 UTM Zone 10N

# View map extent
basemap(limits = lims)

# Process gridded bathy data
rb <- raster_bathymetry(bathy = paste(etopoPath, "ETOPO1_Ice_g_gmt4.grd", sep = "/"),
                        depths = c(50, 100, 200, 300, 500, 1000, 1500, 2000, 4000, 6000, 10000), 
                        proj.out = projection, 
                        boundary = lims
                        )

# Plot the gridded bathy data
class(rb)
names(rb)
raster::plot(rb$raster)

# Vectorize the bathy data
bs_bathy <- vector_bathymetry(rb)
sp::plot(bs_bathy)

# Create shapefiles
bs_land <- clip_shapefile(world, lims)
bs_land <- sp::spTransform(bs_land, CRSobj = sp::CRS(projection))
rgeos::gIsValid(bs_land) # Has to return TRUE, if not use rgeos::gBuffer
bs_land <- rgeos::gBuffer(bs_land, byid = TRUE, width = 0)
sp::plot(bs_land)

# Combine and save shapefiles
save(bs_bathy, bs_land, file = paste(outPath, "bs_shapes.rda", sep = "/"), compress = "xz")



