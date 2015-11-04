#' 990 events of right-wing violence in Germany in 2014 and 2015.
#'
#' A dataset containing georeferenced events of right-wing violence in
#' Germany during the years 2014 and 2015.
#'
#' @format A data frame with 990 rows and 10 variables:
#' \describe{
#'   \item{datum}{date of event}
#'   \item{ort}{city or town of event as reported}
#'   \item{bundesland}{state of event}
#'   \item{kategorie}{type of violence}
#'   \item{zusammenfassung}{short summary of event}
#'   \item{quelle}{primary source}
#'   \item{location}{location as identified by google georeferencing API}
#'   \item{lat}{latitude}
#'   \item{lon}{longitude}
#'   \item{key}{"Kreisschlüssel", unique key to map event to a subregion of Germany}
#' }
#' @source \url{https://www.mut-gegen-rechte-gewalt.de}
"events"
