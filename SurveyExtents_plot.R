library(rstudioapi)
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
library(sp)
library(tmap)

# Set working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefile
extents <- readOGR("SurveyExtent_diss_merge.shp")
coast <- readOGR("WCstates_BC_Mex_100K.shp")
EEZ <- readOGR("pacific_eez_segmented.shp")

# filter on survey
summary(extents)
extents <- extents[extents@data$Survey == "AFSC Triennial",]
extents <- extents[extents@data$Year != "1999",]
extents <- extents[extents@data$Year != "2000",]
extents <- extents[extents@data$Year != "2001",]
extents <- extents[extents@data$Survey != "AFSC Slope",]
extents <- extents[extents@data$Survey != "NWFSC Slope",]
extents <- extents[extents@data$Survey != "NWFSC Shelf-Slope",]
# summary(extents)

# Open a pdf file
pdf(file = "AFSC_Slope_AllYrs.pdf", width = 10, height = 10)
pdf(file = "Triennial_AllYrs.pdf", width = 10, height = 10)
pdf(file = "NWFSC_Slope_AllYrs.pdf", width = 10, height = 6)
# plot
current.mode <- tmap_mode("plot")
tm_shape(extents) + tm_fill("Year", thres.poly = 0) + 
  tm_facets(by = "SurveyYear", nrow = 2, free.coords = FALSE) +
  tm_shape(coast) + tm_polygons() +
  tm_shape(EEZ) + tm_lines() +
  tm_layout(legend.show = FALSE, 
            title.position = c("center", "center"), title.size = 20, 
            panel.label.bg.color = "white", panel.label.fontface = "bold"
            )
# Close the pdf file
dev.off()
