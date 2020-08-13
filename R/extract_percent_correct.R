#' Title
#'
#' @param page xlm2::read_html output
#'
#' @return table of % correct on this question for each group
#' @export
#'
#' @examples
extract_percent_correct <- function(page){
  # leaguewide
  leaguewide <-
    page %>%
    rvest::html_nodes(css = "div.leaguewide") %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d+") %>%
    as.numeric() %>%
    `/`(., 100)

  # leagues
  leagues <-
    page %>%
    rvest::html_nodes(css = "div.score") %>%
    rvest::html_nodes(xpath = "//comment()") %>%
    rvest::html_text() %>%
    grep(pattern = "^<a href='match.php?", value = TRUE) %>%
    sub(x = ., ".*\\d&(.*)'>", "\\1")

  # values
  values <-
    page %>%
    rvest::html_nodes(css = "div.score") %>%
    rvest::html_nodes(xpath = "//comment()") %>%
    rvest::html_text() %>%
    grep(pattern = "^\\s{2}\\d+", value = TRUE) %>%
    as.numeric() %>%
    `/`(., 100)

  pct_correct <-
    tibble(leagues, values) %>%
    rbind(c("League", leaguewide), .)

  return(pct_correct)
}
