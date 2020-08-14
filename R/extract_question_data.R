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

  # best and most common wrong
  wrongs <-
    page %>%
    rvest::html_nodes(css = "h3, div") %>%
    rvest::html_text() %>%
    stringr::str_replace_all("[\\t]+", "")

  best_wrong_idx <-
    wrongs %>%
    grepl(pattern = "Best Wrong Answers", ignore.case = TRUE) %>%
    which() %>%
    max()

  best_wrong <-
    wrongs[best_wrong_idx + 1] %>%
    stringr::str_replace_all("\\n", "")

  most_wrong_idx <-
    wrongs %>%
    grepl(pattern = "Most Common Wrong Answer", ignore.case = TRUE) %>%
    which() %>%
    max()

  most_wrong <-
    wrongs[most_wrong_idx + 1] %>%
    stringr::str_replace_all("\\n", "")

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
