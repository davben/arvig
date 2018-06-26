#' Split events to obtain unique categories.
#'
#' \code{split_events} splits multi-category events from \code{arvig} into
#' observations with unique categories.
#'
#' @param df A data frame. If left unspecified, the \code{arvig} data
#'   frame is used. A user-specified data frame must contain the columns
#'   category_en and category_de.
#' @return A data frame.

#' @examples
#'
#' \dontrun{
#' data(arvig)
#' head(split_events())
#'
#' arvig %>%
#'   filter(stringr::str_detect(category_en, "&")) %>%
#'   split_events()
#' }
#' @export
split_events <- function(df = NULL) {
  if (is.null(df)) {
    df <- arvig::arvig
  }

  # how many helper columns are required?
  numcol <- df %>%
    filter(!is.na(category_en)) %>%
    summarise(multi = max(stringr::str_count(string = category_en, pattern = "&"))) %>%
    .$multi

  # create a character vector of helper column names
  helper_cols <- letters[1:(numcol + 1)]

  # mapping between English and German categories
  cat_df <- distinct(arvig::arvig, category_en, category_de)

  # split multi-category events into multiple rows based on English categories,
  # then merge corresponding German categories.
  df %>%
    separate(category_en, helper_cols, sep = " & ", fill = "right") %>%
    gather(cat, category_en, helper_cols) %>%
    filter(!is.na(category_en)) %>%
    select(-c(cat, category_de)) %>%
    left_join(cat_df, "category_en") %>%
    # change the line below to account for cases with user-specified df that do not contain all of these variables
    select(date, location, state, community_id, longitude, latitude, category_de, category_en, description, source) %>%
    arrange(date)
}
