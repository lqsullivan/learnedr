#' extract_question_data
#'
#' @param page xlm2::read_html output
#'
#' @return table of question data
#' @export
#'
#' @examples
extract_question_data <- function(page){
  # date
  q_date <-
    page %>%
    rvest::html_nodes(css = "h1.matchday") %>%
    rvest::html_text() %>%
    stringr::str_replace(":.*$", "") %>%
    as.Date(format = "%B %d, %Y")

  # question
  question <-
    page %>%
    rvest::html_nodes(css = "div.indivqQuestion") %>%
    rvest::html_text()

  # answer
  answer <-
    page %>%
    rvest::html_nodes(css = "div.indivqAnswerwrapper") %>% # class selector
    rvest::html_nodes(css = "div#xyz") %>% #id selector
    rvest::html_text() %>%
    stringr::str_replace_all("[\\n\\t]+", "")

  # most common wrong answer
  wrongs <-
    page %>%
    rvest::html_nodes(css = "div.indivqBRA-inner") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[\\n\\t]+", "")

  most_wrong <-
    wrongs[1]

  best_wrong <-
    wrongs[2]

  # avg defense
  avg_defense <-
    page %>%
    rvest::html_nodes(css = "div.indivqDefPts") %>%
    rvest::html_text() %>%
    as.numeric()

  # image link
  # music link?
  link <-
    page %>%
    rvest::html_nodes(css = "div.indivqQuestion") %>%
    rvest::html_nodes(css = "a") %>%
    rvest::html_attr("href")
  link <-
    ifelse(identical(link, character(0)), NA, link)

  out <-
    tibble(q_date,
           question,
           answer,
           most_wrong,
           best_wrong,
           avg_defense,
           link)

  return(out)
}
