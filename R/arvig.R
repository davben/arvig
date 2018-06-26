#' ARVIG dataset.
#'
#' A dataset containing 6918 georeferenced events of anti-refugee violence and
#' social unrest in Germany during the years 2014 to 2017.
#'
#' @format A data frame with 6918 rows and 10 variables:
#' \describe{
#'   \item{date}{date of event}
#'   \item{location}{city or town of event as reported}
#'   \item{state}{federal state of event}
#'   \item{community_id}{12-digit community identification number of reported city/town as used by statistical offices}
#'   \item{longitude}{longitude of event}
#'   \item{latitude}{latitude of event}
#'   \item{category_de}{type of event (German)}
#'   \item{category_en}{type of event (English)}
#'   \item{description}{short summary of event}
#'   \item{source}{primary source}
#' }
#' @source \url{https://www.mut-gegen-rechte-gewalt.de}
"arvig"
