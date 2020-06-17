#' get_page
#'
#' @param league integer number of league season (52 is earliest online, 84 is latest as of now)
#' @param day integer number of day in the league (1-25)
#' @param question integer number of question in the day (1-6)
#'
#' @return html extract of page (from rvest::read_html)
#' @export
#'
#' @examples
get_page <- function(league, day, question){
  # add leading zero to day if needed
  day <- ifelse(nchar(day) == 1, paste0("0", day), day)

  # fetch page
  # format https://learnedleague.com/ll60/questions/md01q4.php
  page <-
    paste0("https://learnedleague.com/ll", league,
           "/questions/md", day,
           "/md", day, "q", question, ".php") %>%
    xml2::read_html()

  # TODO: catch 404 error and display better message?

  return(page)
}
