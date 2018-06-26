html_text_collapse <- function(x, trim = FALSE, collapse = "\n"){
  UseMethod("html_text_collapse")
}

html_text_collapse.xml_nodeset <- function(x, trim = FALSE, collapse = "\n"){
  vapply(x, html_text_collapse.xml_node, character(1), trim = trim, collapse = collapse)
}

html_text_collapse.xml_node <- function(x, trim = FALSE, collapse = "\n"){
  paste(xml2::xml_find_all(x, ".//text()"), collapse = collapse)
}

clean_source <- function(x){
  check <- try(html_nodes(x, ".field-name-field-source .field-item"))
  if (inherits(check, "try-error") |
      (length(check)==0)) { return(NA) }

  txt <- check %>%
    html_text_collapse() %>%
    stringr::str_trim()

  links <- html_nodes(check, "a") %>%
    html_attr("href")

  out <- paste(links, collapse = "\n") %>% stringr::str_trim()
  if (nchar(out) == 0) {out <- txt}
  return(out)
}
