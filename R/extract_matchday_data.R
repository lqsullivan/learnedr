#' extract_matchday_data
#'
#' @param page xlm2::read_html output
#'
#' @return table of question data
#' @export
#'
#' @examples
extract_matchday_data <- function(page){
  # category
  category <-
    page %>%
    rvest::html_nodes(css = "div.ind-Q20") %>%
    rvest::html_text() %>%
    stringr::str_replace("[\\n\\tQ\\d\\.]+", "") %>%
    stringr::str_replace("([A-Z/\\s])-.*", "\\1") %>%
    stringr::str_replace("[\\n\\t]+", "") %>%
    stringr::str_replace("\\s$", "")

  return(tibble(category))
}
