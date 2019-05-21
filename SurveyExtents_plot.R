library(rstudioapi)
library(dplyr)
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

# Add field for faceting on geographic subregion (WA, OR, NorCal, CenCal, SoCal)
sabTows <- PacFIN.Logbook.Sab.Tows.May.17.2019 %>%
  mutate(
    region = case_when(
    Best_Lat > 46 ~ "1-Washington",
    Best_Lat <= 46 & Best_Lat > 42 ~ "2-Oregon",
    Best_Lat <= 42 & Best_Lat > 38 ~ "3-NorCal",
    Best_Lat <= 38 & Best_Lat > 34 ~ "4-CenCal",
    Best_Lat <= 34 & Best_Lat > 30 ~ "5-SoCal"
  ))

# Convert PacFIN hauls to shape object
sabPts <- st_as_sf(sabTows, coords = c("Best_Long","Best_Lat"), crs = 4326)

# Create convex hull of positive hauls; need to use function that output spatial object
library(dismo)
sabHull <- dismo::convHull(sabPts)
sabHull <- chull(sabPts)
library(spatstat)
sabHull <- spatstat::convexhull.xy(sabTows$Best_Long, sabTows$Best_Lat)
sabPly <- polygon(sabHull)

# plot
tm_shape(extents) + tm_fill() + 
  tm_shape(sabHull) + tm_fill()

# filter on survey
summary(extents)
extents <- extents[extents@data$Survey == "AFSC Triennial",]
extents <- extents[extents@data$Year != "1999",]
extents <- extents[extents@data$Year != "2000",]
extents <- extents[extents@data$Year != "2001",]
extents <- extents[extents@data$Survey != "AFSC Slope",]
extents <- extents[extents@data$Survey != "NWFSC Slope",]
extents <- extents[extents@data$Survey == "NWFSC Shelf-Slope",]

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

# Open a pdf file
pdf(file = "NWFSC_Comb_wSabTows.pdf", width = 10, height = 8)
# plot with PacFIN hauls as dots
current.mode <- tmap_mode("plot")
tm_shape(extents) + tm_fill() + 
  tm_shape(sabPts) + tm_dots(col = "RYEAR") +
  tm_facets(by = "region", nrow = 2, free.coords = TRUE) +
  tm_shape(coast) + tm_polygons() + tm_text("STATE_NAME", auto.placement = TRUE) +
  tm_shape(EEZ) + tm_lines() +
  tm_layout(legend.show = FALSE, 
            title.position = c("center", "center"), title.size = 20, 
            panel.label.bg.color = "white", panel.label.fontface = "bold"
  )
# Close the pdf file
dev.off()
