library(rstudioapi)
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
library(sp)
library(sf)
library(tmap)

# Set working directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))
print(getwd())

# import shapefiles
extents <- readOGR("SurveyExtent_diss_merge.shp")
coast <- readOGR("WCstates_BC_Mex_100K.shp")
EEZ <- readOGR("pacific_eez_segmented.shp")
sabPts <- st_as_sf(PacFIN.Logbook.Sab.Tows, coords = c("Best_Long","Best_Lat"))
# Add field for faceting on geographic subregion (WA, OR, NorCal, CenCal, SoCal)

# filter on survey
summary(extents)
extents <- extents[extents@data$Survey == "AFSC Triennial",]
extents <- extents[extents@data$Year != "1999",]
extents <- extents[extents@data$Year != "2000",]
extents <- extents[extents@data$Year != "2001",]
extents <- extents[extents@data$Survey != "AFSC Slope",]
extents <- extents[extents@data$Survey != "NWFSC Slope",]
extents <- extents[extents@data$Survey == "NWFSC Shelf-Slope",]
# summary(extents)

# Open a pdf file
pdf(file = "AFSC_Slope_AllYrs.pdf", width = 10, height = 10)
pdf(file = "Triennial_AllYrs.pdf", width = 10, height = 10)
pdf(file = "NWFSC_Slope_AllYrs.pdf", width = 10, height = 6)
# plot
current.mode <- tmap_mode("plot")
tm_shape(extents) + tm_fill("Year", thres.poly = 0) + 
  # tm_facets(by = "SurveyYear", nrow = 2, free.coords = FALSE) +
  tm_shape(sabPts) + tm_dots(col = "RYEAR") +
  tm_shape(coast) + tm_polygons() +
  tm_shape(EEZ) + tm_lines() +
  tm_layout(legend.show = FALSE, 
            title.position = c("center", "center"), title.size = 20, 
            panel.label.bg.color = "white", panel.label.fontface = "bold"
            )

# Found this code in nwfscMapping package. Doesn't work yet.
library(nwfscMapping)
multiMap(nrows=1,ncols=2,lonLims=matrix(c(-125,-125,-122,-123),nrow=2),latLims=matrix(c(42,45,45,46),nrow=2),fnc=plotMap,polys=extents)
multiMap(nrows=1,ncols=3,
         lonLims=matrix(c(-123.1,-122.9,-125,-121,-125,-121),nrow=3,byrow=T),
         latLims=matrix(c(37,40,40,45,45,48),nrow=3,byrow=T),
         fnc=plotMap,polys=extents)
# Close the pdf file
dev.off()
