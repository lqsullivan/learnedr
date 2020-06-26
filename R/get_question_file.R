#' Title
#'
#' @param link link to associated file
#'
#' @return NULL, invisibly (also downloads a file to ./question_files/ and makes that dir if it doesn't exist)
#' @export
#'
#' @examples
get_question_file <- function(link){
  # create directory if doesn't exist
  if(!dir.exists("./question_files")){
    dir.create("./question_files")
  }

  download.file(url      = paste0("https://learnedleague.com", link),
                destfile = paste0("./question_files/", sub("/.*/", "", link)),
                method   = "auto",
                mode     = "wb",
                quiet    = TRUE)

  invisible(NULL)
}
