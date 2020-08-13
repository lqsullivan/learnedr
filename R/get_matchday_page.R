#' get_matchday_page
#'
#' @param league integer number of league season (52 is earliest online, 84 is latest as of now)
#' @param day integer number of day in the league (1-25)
#'
#' @return html extract of page (from rvest::read_html)
#' @export
#'
#' @examples
get_matchday_page <- function(league, day){
  # add leading zero to day if needed
  day <- ifelse(nchar(day) == 1, paste0("0", day), day)

  # fetch page
  # format https://learnedleague.com/match.php?52&1
  page <-
    paste0("https://learnedleague.com/match.php?", league, "&", day) %>%
    xml2::read_html()

  # TODO: catch 404 error and display better message?

  return(page)
}
