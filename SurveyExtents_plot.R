library(rstudioapi)
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
library(sp)

# Set working directory
current_path <- getActiveDocumentContext()$path
# setwd("/Users/curt.whitmire/Documents/GIS/AnalysisProgram/Assessment/Assessment2019/layers")
setwd(dirname(current_path))
print(getwd())

# # Getting rid of anything saved in your workspace 
rm(list=ls())

# import shapefile
extents <- readOGR("SurveyExtent_diss_merge.shp")

summary(extents)
extents <- extents[extents@data$Survey == "NWFSC Slope",]
extents <- extents[extents@data$Year != "1999",]
extents <- extents[extents@data$Year != "2000",]
extents <- extents[extents@data$Year != "2001",]
summary(extents)

library(tmap)
# vignette(package = "tmap") # available vignettes in tmap
# vignette("tmap-nutshell")

# plot
current.mode <- tmap_mode("plot")
tmap_style("white")
# current.mode <- tmap_mode("view")
# tm_basemap(leaflet::providers$Esri.WorldShadedRelief) +
tm_shape(extents) +
  tm_fill("Year", thres.poly = 0) +
  tm_facets("SurveyYear", nrow = 1, free.coords = FALSE) +
  tm_layout(legend.show = FALSE, title.position = c("center", "center"), title.size = 20)

library(ggplot2)
library(ggmap)
# plot