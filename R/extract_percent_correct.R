#' Title
#'
#' @param page xlm2::read_html output
#'
#' @return table of % correct on this question for each group
#' @export
#'
#' @examples
extract_percent_correct <- function(page){
  # percent correct
  pct_correct <-
    page %>%
    rvest::html_nodes(css = "div.indivqRulerContainer") %>%
    rvest::html_text() %>%
    stringr::str_remove_all("[:%]") %>%
    stringr::str_split("[\\r\\n\\t]+") %>%
    unlist() %>%
    `[`(1:(length(.) - 2)) %>%
    stringr::str_split("\\s(?=\\d+)") %>%
    do.call(what = rbind) %>%
    as.data.frame() %>%
    select(group = V1,
           pct   = V2) %>%
    mutate(pct   = as.numeric(pct)/100)

  return(pct_correct)
}
