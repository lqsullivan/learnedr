#' get_question_page
#'
#' @param league integer number of league season (52 is earliest online, 84 is latest as of now)
#' @param day integer number of day in the league (1-25)
#' @param question integer number of question in the day (1-6)
#'
#' @return html extract of page (from rvest::read_html)
#' @export
#'
#' @examples
get_question_page <- function(league, day, question){
  # fetch page
  # format https://learnedleague.com/question.php?52&1&1
  page <-
    paste0("https://learnedleague.com/question.php?",
           league, "&", day, "&", question) %>%
    xml2::read_html()

  # TODO: catch 404 error and display better message?

  return(page)
}
