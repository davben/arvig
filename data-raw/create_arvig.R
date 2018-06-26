library(tidyverse)
library(rvest)
library(ggmap)
library(sf)

# events from 2014 --------------------------------------------------------

# Events from 2014 are not part of the same web-chronicle as all subsequent
# years and the information is not neatly listed using HTML-markup for
# different variables. Therefore these events were copy-pasted and cleaned
# manually.
# Source: https://www.mut-gegen-rechte-gewalt.de/news/meldung/rechte-hetze-gegen-fluechtlinge-eine-chronik-der-gewalt-2014-03

load("data-raw/events_2014.rda")


# scrape events from 2015 onwards -----------------------------------------
# events_2015_later <- read_chronicle(2015:2017)
# save(events_2015_later, file = "data-raw/events_2015_later.rda", compress = "xz")
load("data-raw/events_2015_later.rda")

## separate strings in cases of more than one category per event
## does not seem to be relevant anymore, all events are distinctly categorized
# events_2015_later$category_de <- gsub("([a-z])([A-Z])","\\1 & \\2", events_2015_later$category_de)


# combine and fix errors --------------------------------------------------
events <- bind_rows(events_2014, events_2015_later) %>%
  arrange(date) %>%
  mutate(location = stringr::str_replace(location, "Massow", "Massow (LDS)"),
         location = stringr::str_replace(location, "Haspe \\(Hagen\\)", "Haspe, Hagen"),
         location = stringr::str_replace(location, "Marke, Raguhn-Jeßnitz", "Marke"),
         location = stringr::str_replace(location, "Kirchheim im Schwarzwald", "Kirchheim in Schwaben"),
         state = ifelse(stringr::str_detect(location, "Halle") & state == "Sachsen", "Sachsen-Anhalt", state),
         location = ifelse(stringr::str_detect(location, "Halle") & state == "Sachsen-Anhalt", "Halle (Saale)", location),
         location = ifelse(stringr::str_detect(location, "Halle") & state == "Nordrhein-Westfalen", "Halle (Westfalen)", location),
         location = ifelse(location == "Wittenberge" & state == "Sachsen-Anhalt", "Wittenberg", location),
         state = ifelse(location == "Geldern" & state == "Niedersachsen", "Nordrhein-Westfalen", state),
         state = ifelse(location == "Goslar" & state == "Hessen", "Niedersachsen", state),
         location = ifelse(location == "Berlin-Hohenschönhausen", "Neu-Hohenschönhausen, Berlin", location),
         location = ifelse(location == "Hohenschönhausen, Berlin" & date == lubridate::ymd("2015-12-22"), "Zingster Str./Ribnitzer Str., Berlin", location),
         location = ifelse(location == "Einsiedel", "Einsiedel, Chemnitz", location),
         location = ifelse(location == "Merkers", "Merkers-Kieselbach", location),
         state = ifelse(location == "Naumburg" & state == "Sachsen", "Sachsen-Anhalt", state),
         location = ifelse(stringr::str_detect(location, "Naumburg") & state == "Sachsen-Anhalt", "Naumburg (Saale)", location),
         state = ifelse(location == "Leverkusen" & state == "Niedersachsen", "Nordrhein-Westfalen", state),
         state = ifelse(location == "Röttenbach" & state == "Baden-Württemberg", "Bayern", state),
         state = ifelse(location == "Hildesheim" & state == "Baden-Württemberg", "Niedersachsen", state),
         location = ifelse(location == "Berline" & date == lubridate::ymd("2017-02-21"), "Berlin", location),
         state = ifelse(location == "Dessau" & state == "Sachsen", "Sachsen-Anhalt", state)) %>%
  filter(!(location == "Sebnitz" & state == "Thüringen"),
         !(location == "Nümbrecht" & state == "Niedersachsen"),
         !(location == "Mainz" & state == "Hessen")) %>% # not clear whether Mainz (RP) or parts of Wiesbaden (HE)
  mutate_if(is.character, enc2utf8) %>% # seems to be necessary for subsequent calls to Google Maps API
  filter(!is.na(location)) # those 2 events from 2014 without a clear location (set of NPD demonstrations) need to be sorted out!


# geocode -----------------------------------------------------------------
### check to see, if coordinates for a given location have been retrieved
### in the past in order to reduce the number of API calls.

load("./data-raw/geocode_db.rda")

unique_locations <- events %>%
  distinct(location, state) %>%
  unite(address, location, state, sep = ", ") %>%
  # Without this change, Google provides the coordinates of Berlin, Maryland:
  mutate(address = ifelse(address == "Berlin, Berlin", "Berlin, Deutschland", address)) %>%
  anti_join(geocode_db, "address")

if (nrow(unique_locations) > 0) {
  unique_locations <- unique_locations %>%
    mutate_geocode(address, source = "google")
  geocode_db <- bind_rows(geocode_db, unique_locations)
  save(geocode_db, file = "./data-raw/geocode_db.rda", compress = "xz")
}


events <- events %>%
  unite(address, location, state, sep = ", ", remove = FALSE) %>%
  mutate(address = ifelse(address == "Berlin, Berlin", "Berlin, Deutschland", address)) %>%
  left_join(geocode_db, "address") %>%
  select(-address)



# assign administrative id based on coordinates ---------------------------
if(file.exists("./data-raw/germany.rda")) {
  load("./data-raw/germany.rda")
} else {
  source("./data-raw/germany.R")
}

events <- events %>%
  # necessary because the google coordinate appears to be the centroid,
  # which is just outside the polygon.
  mutate(lon = ifelse(location == "Wismar", 11.454086, lon),
         lat = ifelse(location == "Wismar", 53.894706, lat)) %>%
  st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326, agr = "constant") %>%
  st_transform(crs = st_crs(germany)) %>%
  st_intersection(select(germany, RS) %>% st_set_agr("constant")) %>%
  st_set_geometry(NULL)


# clean data frame --------------------------------------------------------

## add english event categories
category_df <- tibble::tibble(category_de = c("Brandanschlag", "Sonstige Angriffe",
                                              "Kundgebung/Demo", "Tätlicher Übergriff/Körperverletzung",
                                              "Verdachtsfall",
                                              "Brandanschlag & Sonstige Angriffe",
                                              "Kundgebung/Demo & Sonstige Angriffe",
                                              "Kundgebung/Demo & Tätlicher Übergriff/Körperverletzung",
                                              "Sonstige Angriffe & Tätlicher Übergriff/Körperverletzung",
                                              "Kundgebung/Demo & Sonstige Angriffe & Tätlicher Übergriff/Körperverletzung"),
                              category_en = c("arson", "miscellaneous attack", "demonstration", "assault", "suspicion",
                                              "arson & miscellaneous attack", "demonstration & miscellaneous attack",
                                              "demonstration & assault", "miscellaneous attack & assault",
                                              "demonstration & miscellaneous attack & assault"))


arvig <- events %>%
  left_join(category_df, "category_de") %>%
  mutate(community_id = as.character(RS)) %>%
  replace_na(list(category_de = "sonstige", category_en = "other")) %>%
  select(date, location, state, community_id, longitude = lon, latitude = lat, category_de, category_en, description, `source`) %>%
  arrange(date, community_id)

#save(arvig, file = "./data/arvig.rda", compress = "xz")
