# administrative boundaries for Germany -----------------------------------
## Source: Geodatenzentrum

library(rgdal)
src <- "http://sg.geodatenzentrum.de/web_download/vg/vg1000-ew_3112/utm32s/shape/vg1000-ew_3112.utm32s.shape.ebenen.zip"
lcl <- "data-raw/germany_shape.zip"

if (!file.exists(lcl)) {
  download.file(src, lcl)
}
unzip(lcl, exdir = "data-raw/germany_shape/")

germany <- readOGR(dsn = "data-raw/germany_shape/vg1000-ew_3112.utm32s.shape.ebenen/vg1000-ew_ebenen", layer = "VG1000_KRS")
germany <- spTransform(germany, CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs"))

save(germany, file = "./data-raw/germany.Rdata")
