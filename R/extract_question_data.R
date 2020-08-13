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
    as.Date(tryFormats = c("%b. %d, %Y", "%B %d, %Y"))

  # question
  question <-
    page %>%
    rvest::html_nodes(css = "div.indivqQuestion") %>%
    rvest::html_text() %>%
    stringr::str_replace("[\\n|\\t]+", "")

  # answer
  answer <-
    page %>%
    rvest::html_nodes(css = "div.indivqAnswerwrapper_n") %>% # class selector
    rvest::html_nodes(css = "div#xyz") %>% #id selector
    rvest::html_text() %>%
    stringr::str_replace_all("[\\n\\t]+", "")

  # image link
  # music link?
  link <-
    page %>%
    rvest::html_nodes(css = "div.indivqQuestion") %>%
    rvest::html_nodes(css = "a") %>%
    rvest::html_attr("href")
  link <-
    ifelse(identical(link, character(0)), NA, link)

  # some values discontinued as of august 2020 (keeping it for my db)
  # most common wrong answer
  wrongs <-
    page %>%
    rvest::html_nodes(css = "h3, div") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[\\t]+", "")

  best_wrong <-
    wrongs %>%
    grep(pattern = "^\n\nBest Wrong Answers", ignore.case = TRUE, value = TRUE) %>%
    stringr::str_replace("^\n+.*\n+", "") %>%
    stringr::str_replace("\n+$", "")
  best_wrong <-
    ifelse(identical(best_wrong, character(0)), NA, best_wrong)

  # far as i know, this is gone
  most_wrong <-
    wrongs %>%
    grep(pattern = "^\n\nMost Common Wrong Answers", ignore.case = TRUE, value = TRUE) %>%
    stringr::str_replace("^\n+.*\n+", "") %>%
    stringr::str_replace("\n+$", "")
  most_wrong <-
    ifelse(identical(most_wrong, character(0)), NA, most_wrong)

  # avg defense
  avg_defense <-
    page %>%
    rvest::html_nodes(css = "div.indivqContent") %>%
    rvest::html_nodes(xpath = "//comment()") %>%
    rvest::html_text() %>%
    grep(pattern = "defense", value = TRUE) %>%
    stringr::str_replace_all("[\\n\\t]+", "") %>%
    xml2::read_html() %>%
    rvest::html_nodes(css = "div") %>%
    rvest::html_text() %>%
    stringr::str_replace("Def: ", "") %>%
    as.numeric()

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
