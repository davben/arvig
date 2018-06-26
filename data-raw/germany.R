# municipal boundaries for Germany -----------------------------------
## Source: Geodatenzentrum http://www.bkg.bund.de

src <- "http://sg.geodatenzentrum.de/web_download/vg/vg250-ew_3112/utm32s/shape/vg250-ew_3112.utm32s.shape.ebenen.zip"
lcl <- "./data-raw/germany_shape.zip"

if (!file.exists(lcl)) {
  download.file(src, lcl)
}
unzip(lcl, exdir = "./data-raw/germany_shape")

germany <- sf::st_read("./data-raw/germany_shape/vg250-ew_3112.utm32s.shape.ebenen/vg250-ew_ebenen/VG250_GEM.shp")
save(germany, file = "./data-raw/germany.rda", compress = "xz")
