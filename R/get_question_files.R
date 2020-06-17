#' Title
#'
#' @param id
#' @param file_link
#'
#' @return NOTE: this violates functional programming principles, maybe a better way
#' @export
#'
#' @examples
get_question_files <- function(id, file_link){
  file_ext <-
    file_link %>%
    str_extract("\\..*+$")

  for(i in which(!is.na(file_link))){
    download.file(url      = paste0("https://learnedleague.com", file_link[i]),
                  destfile = paste0("./question_pics/", id[i], file_ext),
                  quiet    = TRUE,
                  mode     = "wb")
  }

  invisible(NULL)
}
