# administrative boundaries for Germany -----------------------------------
## Source: Geodatenzentrum

library(rgdal)
src <- "http://sg.geodatenzentrum.de/web_download/vg/vg250_0101/utm32s/shape/vg250_0101.utm32s.shape.ebenen.zip"
lcl <- "data-raw/germany_shape.zip"

if (!file.exists(lcl)) {
  download.file(src, lcl)
}
unzip(lcl, exdir = "data-raw/germany_shape/")

germany <- readOGR(dsn = "data-raw/germany_shape/vg250_0101.utm32s.shape.ebenen/vg250_ebenen", layer = "VG250_GEM")
germany_250 <- spTransform(germany, CRS("+proj=longlat +ellps=GRS80 +datum=WGS84 +no_defs"))

save(germany_250, file = "./data-raw/germany_250.Rdata")
