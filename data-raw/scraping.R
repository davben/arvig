#' Scrape Events of Anti-Refugee Violence from "Mut gegen rechte Gewalt" Chronicle.
#'
#' This function is built only for one specific purpose:
#' Scraping the website \url{https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle}
#' for all events a specified year (currently, 2015 and 2016). It retrieves the date, location,
#' bundesland, category, summary and source for each event and returns a data frame.
#'
#' @param years numeric vector of years for which to retrieve data from the chronicle.
#' @return A data frame of events as listed on the website, consisting of columns for date, location, bundesland,
#'    category, summary and source.
#' @examples
#' \dontrun{
#' chronicle <- read_chronicle(c(2015, 2016))
#' }
read_chronicle <- function(years) {
  if (!requireNamespace("purrr", quietly = TRUE)) {
    stop("purrr is needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("lubridate is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  years %>%
    purrr::map_df(read_chronicle_year) %>%
    mutate(date = lubridate::dmy(date))
}

#' Scrape Events of Anti-Refugee Violence in a Given Year.
#'
#' This function scrapes all events in a specified year from the "Mut gegen rechte Gewalt" chronicle.
#' It retrieves the date, location,
#' bundesland, category, summary and source for each event and returns a data frame.
#'
#' @param year numeric value specifying the year for which to retrieve data from the chronicle.
#'    Currently, it only works with 2015 or 2016.
#' @return A data frame of events from a single year as listed in the chronicle,
#'    consisting of columns for date, location, bundesland, category, summary and source.
read_chronicle_year <- function(year) {
  if (!requireNamespace("rvest", quietly = TRUE)) {
    stop("rvest is needed for this function to work. Please install it.",
         call. = FALSE)
  } else if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("stringi is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  current_year <- lubridate::year(Sys.Date())
  if (!(year >= 2015)) stop("Data can only be retrieved from the year 2015 onwards.")
  if (!(year <= current_year)) stop("The chronicle does not contain events from the future.")

  base_page <- paste0("https://mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?&&field_date_value[value][year]=", year,"&page=0")
  chronicle <- xml2::read_html(base_page)

  last_page <- chronicle %>%
    html_node(".pager-last a") %>%
    html_attr("href") %>%
    stringi::stri_extract(regex = "page=[0-9]+") %>%
    gsub("page=", "", .) %>%
    as.numeric()

  page_seq <- 0:last_page

  print(paste0("Retrieving data for the year ", year, " from ", last_page + 1, " pages."))

  out <- page_seq %>% purrr::map_df(read_chronicle_page, chronicle_year = year)
  out
}


#' Scrape Individual Chronicle Page for Events of Anti-Refugee Violence in a Given Year.
#'
#' This function scrapes a single page from the "Mut gegen rechte Gewalt" chronicle.
#' It retrieves the date, location,
#' bundesland, category, summary and source for each event and returns a data frame.
#'
#' @param chronicle_year numeric value specifying the year for which to retrieve data from the chronicle.
#'    Currently, it only works with 2015 or 2016.
#' @param page_nr numeric value specifying chronicle page from which to retrieve data.
#' @return A data frame of events from a single chronicle page as listed in the chronicle,
#'    consisting of columns for date, location, bundesland, category, summary and source.
read_chronicle_page <- function(page_nr, chronicle_year) {
  violence <- xml2::read_html(paste0("https://www.mut-gegen-rechte-gewalt.de/service/chronik-vorfaelle?field_date_value[value][year]=", chronicle_year,"&page=", page_nr))

  location <- violence %>%
    html_nodes(".field-name-field-city") %>%
    html_text()

  state <- violence %>%
    html_nodes(".field-name-field-bundesland") %>%
    html_text()

  source_raw <- violence %>%
    html_nodes(".node-chronik-eintrag")

  source <- purrr::map_chr(source_raw, clean_source)

  date <- violence %>%
    html_nodes(".field-name-field-date") %>%
    html_text()

  category_de <- violence %>%
    html_nodes(".field-name-field-art") %>%
    html_text()

  description <- violence %>%
    html_nodes(".field-type-text-with-summary") %>%
    html_text()

  result <- tibble::tibble(date, location, state, category_de, description, source) %>%
    mutate_all(funs(gsub("^\\s+|\\s+$", "", .)))
}
