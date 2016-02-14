library(plyr)
library(dplyr)
library(rvest)
library(rgdal)
library(maptools)

# events from 2014 --------------------------------------------------------
load("data-raw/events_2014.Rdata")
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
         kategorie = gsub("(S) & (K)", "Sonstige Angriffe auf Unterkünfte & Tätlicher Übergriff/Körperverletzung", kategorie, fixed=TRUE),
         quelle = gsub("Quelle: ", "", quelle))

events_2014[events_2014$datum == "22.01.2014" & events_2015$ort == "Bad Dürheim", ]$ort <- "Bad Dürrheim"
events_2014[events_2014$datum == "22.01.2014" & events_2015$ort == "Bad Dürrheim", ]$bundesland <- "Baden-Württemberg"

# Under Windows: Fix encoding again: even though this script is supposed to be utf-8, the above substitutions are not.
if (.Platform$OS.type == "windows") {
  events_2014$kategorie <- iconv(events_2014$kategorie, from = "latin1", to = "utf8")
}


## geocode all events from 2014
# locations <- paste(events_2014$ort, events_2014$bundesland, sep = ", ")
## locations <- enc2utf8(locations) # necessary for locations containing umlauts
# geocodes_2014 <- ggmap::geocode(locations, output = "all", source = "google", nameType = "long")
# save(geocodes_2014, file = "data-raw/geocodes_2014.Rdata")

load("data-raw/geocodes_2014.Rdata")
geocodes_2014_df <- ldply(geocodes_2014, extract_from_geocode)
events_2014 <- tbl_df(cbind(events_2014, geocodes_2014_df))


# Due to ambiguity in the documentation of the event, the following observation is excluded:
events_2014 <- filter(events_2014, !(ort=="Heidenheim"), !(datum=="07.01.2014"))

# events from 2015 --------------------------------------------------------

# scrape the website (save data for quicker use)
# events_2015 <- ldply(c(0:94), read_data, .progress="text")
# save(events_2015, file = "data-raw/events_2015.Rdata")
load("data-raw/events_2015.Rdata")
events_2015 <- colwise(function(x)iconv(x, from = "utf8", to = "utf8"))(events_2015) # clean up encoding

# separate strings in cases of more than one category per event
events_2015$kategorie <- gsub("([a-z])([A-Z])","\\1 & \\2", events_2015$kategorie)

# Provide additional information for correct geo-coding:
events_2015[events_2015$datum == "01.09.2015" & events_2015$ort == "Massow", ]$ort <- "Massow (LDS)"

## geocode all events from 2015
# locations <- paste(events_2015$ort, events_2015$bundesland, sep = ", ")
# geocodes_2015 <- ggmap::geocode(locations, output = "all", source = "google", nameType = "long")
# save(geocodes_2015, file = "./data-raw/geocodes_2015.Rdata")

load("data-raw/geocodes_2015.Rdata")
geocodes_2015_df <- ldply(geocodes_2015, extract_from_geocode)
events_2015 <- tbl_df(cbind(events_2015, geocodes_2015_df))


# create combined data frame ----------------------------------------------
events <- rbind(events_2014, events_2015)


# include „Kreisschlüssel“ for each event ---------------------------------
## use shapefile to determine the respective "Kreisschlüssel" for each event.
load("data-raw/germany_250.Rdata")

# assign temporary event ID
events$id <- 1:nrow(events)
# map events to subregions of Germany
keys <- check_polygons(germany_250, events[ ,c("id", "lon", "lat")], key = "RS")

events <- events %>%
  left_join(keys, "id") %>%
  mutate(community_id = ifelse(ort == "Wismar", "130740087087", community_id)) %>% # necessary because the geocoded point is slightly outside the polygon
  select(-id)


# clean variable names ----------------------------------------------------
events <- events %>%
  mutate(date = dmy(datum)) %>%
  rename(location = ort,
         state = bundesland,
         description = zusammenfassung,
         `source` = quelle,
         longitude = lon,
         latitude = lat) %>%
  mutate(category_en = ifelse(kategorie == "Brandanschlag", "arson",
                           ifelse(kategorie == "Sonstige Angriffe auf Unterkünfte", "miscellaneous attack",
                                  ifelse(kategorie == "Kundgebung/Demo", "demonstration",
                                         ifelse(kategorie == "Tätlicher Übergriff/Körperverletzung", "assault",
                                                ifelse(kategorie == "Brandanschlag & Sonstige Angriffe auf Unterkünfte", "arson & miscellaneous attack",
                                                       ifelse(kategorie == "Kundgebung/Demo & Sonstige Angriffe auf Unterkünfte", "demonstration & miscellaneous attack",
                                                              ifelse(kategorie == "Kundgebung/Demo & Tätlicher Übergriff/Körperverletzung", "demonstration & assault",
                                                                     ifelse(kategorie == "Sonstige Angriffe auf Unterkünfte & Tätlicher Übergriff/Körperverletzung", "miscellaneous attack & assault",
                                                                            ifelse(kategorie == "Kundgebung/Demo & Sonstige Angriffe auf Unterkünfte & Tätlicher Übergriff/Körperverletzung", "demonstration & miscellaneous attack & assault", kategorie)))))))))) %>%
  rename(category_de = kategorie) %>%
  select(date, location, state, community_id, longitude, latitude, category_de, category_en, description, `source`)


#save(events, file = "./data/events.Rda")
