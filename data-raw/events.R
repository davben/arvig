library(plyr)
library(dplyr)
library(rvest)
library(rgdal)

# events from 2014 --------------------------------------------------------
load("data-raw-raw/events_2014.Rdata")
events_2014 <- colwise(function(x)iconv(x, to = "utf8", from = "latin1"))(events_2014)

events_2014 <- events_2014 %>%
  mutate(bundesland = gsub("(", "", bundesland, fixed=TRUE),
         bundesland = gsub(")", "", bundesland, fixed=TRUE),
         bundesland = gsub("Bayer", "Bayern", bundesland, fixed=TRUE),
         bundesland = gsub("Bayernn", "Bayern", bundesland, fixed=TRUE),
         bundesland = gsub("Rheinland Pfalz", "Rheinland-Pfalz", bundesland, fixed=TRUE),
         bundesland = gsub("NRW", "Nordrhein-Westfalen", bundesland, fixed=TRUE),
         kategorie = gsub("(S) & (A)", "(A) & (S)", kategorie, fixed=TRUE),
         kategorie = gsub("(K) & (S)", "(S) & (K)", kategorie, fixed=TRUE),
         kategorie = gsub("(B) & (K)", "(K)", kategorie, fixed=TRUE),
         kategorie = gsub("(B) & (S)", "(S)", kategorie, fixed=TRUE),
         kategorie = gsub("(D) & (B)", "(D)", kategorie, fixed=TRUE),
         kategorie = gsub("(D) & (P)", "(D)", kategorie, fixed=TRUE),
         kategorie = gsub("(K) & (B)", "(K)", kategorie, fixed=TRUE),
         kategorie = gsub("(P) & (S)", "(S)", kategorie, fixed=TRUE),
         kategorie = gsub("(S) & (B)", "(S)", kategorie, fixed=TRUE),
         kategorie = gsub("(S) & (P)", "(S)", kategorie, fixed=TRUE),
         kategorie = gsub("(B)", NA, kategorie, fixed=TRUE),
         kategorie = gsub("(P)", NA, kategorie, fixed=TRUE),
         kategorie = gsub("(B) & (P)", NA, kategorie, fixed=TRUE),
         kategorie = gsub("(A)", "Brandanschlag", kategorie, fixed=TRUE),
         kategorie = gsub("(D)", "Kundgebung/Demo", kategorie, fixed=TRUE),
         kategorie = gsub("(K)", "Tätlicher Übergriff/Körperverletzung", kategorie, fixed=TRUE),
         kategorie = gsub("(S)", "Sonstige Angriffe auf Unterkünfte", kategorie, fixed=TRUE),
         kategorie = gsub("(A) & (S)", "Brandanschlag & Sonstige Angriffe auf Unterkünfte", kategorie, fixed=TRUE),
         kategorie = gsub("(D) & (S)", "Kundgebung/Demo & Sonstige Angriffe auf Unterkünfte", kategorie, fixed=TRUE),
         kategorie = gsub("(D) & (K)", "Kundgebung/Demo & Tätlicher Übergriff/Körperverletzung", kategorie, fixed=TRUE),
         kategorie = gsub("(S) & (K)", "Sonstige Angriffe auf Unterkünfte & Tätlicher Übergriff/Körperverletzung", kategorie, fixed=TRUE))


## geocode all events from 2014
# locations <- paste(events_2014$ort, events_2014$bundesland, sep = ", ")
# locations <- enc2utf8(locations) # necessary for locations containing umlauts
# geocodes_2014 <- ggmap::geocode(locations, output = "all", source = "google", nameType = "long")
# save(geocodes_2014, file = "data-raw/geocodes_2014.Rdata")

load("data-raw/geocodes_2014.Rdata")
geocodes_2014_df <- ldply(geocodes_2014, extract_from_geocode)
events_2014 <- tbl_df(cbind(events_2014, geocodes_2014_df))


# events from 2015 --------------------------------------------------------

# scrape the website (save data for quicker use)
# events_2015 <- ldply(0:49, read_data)
# save(events_2015, file = "data-raw/events_2015.Rdata")
load("data-raw/events_2015.Rdata")

# repair minor issues of concatenated strings and spelling mistakes
events_2015$kategorie <- gsub("([a-z])([A-Z])","\\1 & \\2", events_2015$kategorie)
events_2015$quelle <- gsub("([a-z])([A-Z])","\\1 & \\2", events_2015$quelle)
events_2015$quelle <- gsub("(twitter)"," & \\1", events_2015$quelle)
events_2015$quelle <- gsub("(rosenheim24\\.de)"," & \\1", events_2015$quelle)
events_2015$ort <- gsub("Göditz","Gröditz", events_2015$ort, fixed = TRUE)
events_2015[events_2015$ort == "Zeithain", ]$bundesland <- "Sachsen"
events_2015[events_2015$ort == "Weiskirchen", ]$bundesland <- "Saarland"


## geocode all events from 2015
# locations <- paste(events_2015$ort, events_2015$bundesland, sep = ", ")
# locations <- enc2utf8(locations) # necessary for locations containing umlauts
# geocodes_2015 <- ggmap::geocode(locations, output = "all", source = "google", nameType = "long")
# save(geocodes_2015, file = "./data-raw/geocodes_2015.Rdata")

load("data-raw/geocodes_2015.Rdata")
geocodes_2015_df <- ldply(geocodes_2015, extract_from_geocode)
events_2015 <- tbl_df(cbind(events_2015, geocodes_2015_df))


# create combined data frame ----------------------------------------------
events <- rbind(events_2014, events_2015)


# include „Kreisschlüssel“ for each event ---------------------------------
## use shapefile to determine the respective "Kreisschlüssel" for each event.
load("data-raw/germany.Rdata")

# assign temporary event ID
events$id <- 1:nrow(events)
# map events to subregions of Germany
keys <- check_polygons(germany, events[ ,c("id", "lon", "lat")], .key = "RS")

events <- events %>%
  left_join(keys, "id") %>%
  mutate(key = ifelse(ort == "Wismar", "13074", key)) %>% # necessary because the geocoded point is slightly outside the polygon
  select(-id)

save(events, file = "./data/events.Rda")
