#' Title
#'
#' @param id character question id (format ll##md##q#)
#'
#' @return vector with league, match day, and question numbers for each question id
#' @export
#'
#' @examples
extract_id_nums <- function(id){
  stringr::str_extract_all(id, "\\d+")[[1]]
}
