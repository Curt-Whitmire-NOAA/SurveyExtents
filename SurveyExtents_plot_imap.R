library(rstudioapi)
library(rgeos) # "Geomegry Engine- Open Source (GEOS)"
library(rgdal) # "Geospatial Data Analysis Library (GDAL)"
library(sp)

# Set working directory
current_path <- getActiveDocumentContext()$path
# setwd("/Users/curt.whitmire/Documents/GIS/AnalysisProgram/Assessment/Assessment2019/layers")
setwd(dirname(current_path))
print(getwd())

# import shapefile
extents <- readOGR("SurveyExtent_diss_merge.shp")

summary(extents)
extents <- extents[extents@data$Survey == "NWFSC Slope",]
extents <- extents[extents@data$Year != "1999",]
extents <- extents[extents@data$Year != "2000",]
extents <- extents[extents@data$Year != "2001",]

# eastLongitude <- -122 - (N + 1) * longitudeDelta
eastLongitude <- eastLongitude # Needed for imap() to find this

latExtend <- ifelse(N > 13, -((-125 - (N + 1) * 3.5 + 117) - (-125 - 14 * 3.5 + 117))/3, 0)

Imap::imap(longlat = list(Imap::world.h.land, Imap::world.h.borders), col= c("black", "cyan"), poly = c("grey40", NA), longrange = c(eastLongitude, -117), latrange = c(27 - latExtend, 48.2), 
           axes = 'latOnly', zoom = FALSE, bg = "white")
box(lwd = 5)

Col <- colorRampPalette(colors = c("blue", "dodgerblue", "cyan", "green", "orange", "red", "red3"))

COL <- Col(13)[SP.Results$Rescaled.Sum]
JRWToolBox::hexPolygon(SP.Results$X, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep=NA), col = COL, border = COL)

for (i in 1:N) {
  COL <- Col(13)[SP.Results[, N + 3 - i]]
  JRWToolBox::hexPolygon(SP.Results$X - i * longitudeDelta, SP.Results$Y, hexC = hexcoords(dx = 0.1, sep=NA), col = COL, border = COL)
}

Index.$LongPlotValues <- -124.6437 + seq(-longitudeDelta, by = -longitudeDelta, len = N)
Index.$LatPlotValues <- rev((48 - 34.2) * (Index.$Estimate_metric_tons - min(Index.$Estimate_metric_tons))/max(Index.$Estimate_metric_tons) + 34.2)
Index.$LatSD_mt <- rev((48 - 34.2)/(max(Index.$Estimate_metric_tons) - min(Index.$Estimate_metric_tons)) * Index.$SD_mt)