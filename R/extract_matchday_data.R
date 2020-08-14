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
    stringr::str_replace("\\s$", "") %>%
    # league 57 has class 1 and 2, we're skipping class 2 cause there's no
    # question page
    `[`(1:6)

  return(tibble(category))
}
