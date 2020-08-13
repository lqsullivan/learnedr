#' update_question_db
#'
#' Updates the database of questions (and percent correct answers) in './learned_league.sqlite'
#'
#' @param type how to update the database (see details for more info)
#'
#' @return
#' @export
#'
#' @examples
#'
#' @details type parameter has 2 valid arguments (right now)
#' * `next_league` searches the database for the highest league number (52+) of a question that's already downloaded and generates question ids for the next highest-numbered league
#' * `fill_gaps` searches the database for missing questions (each league has 25 days and 6 questions per) and gets the first 150 of them
#' * `continuous` (planned, not implemented) pulls the next question as you're answering one in play mode
#' * `by_id` (planned, not implemented) pulls specific questions by id
#'
update_question_db <- function(type = "next_league"){
  # connect to db
  con <- DBI::dbConnect(drv    = RSQLite::SQLite(),
                        dbname = "./learned_league.sqlite")

  # identify questions to pull
  q_ids <-
    if (type == "next_league") {
      next_league(con)
    } else if (type == "fill_gaps") {
      fill_gaps(con)
    } else if (type == "continuous") {
      error("continuous update type not implemented")
    } else if (type == "by_id") {
      error("by id type not implemented")
    } else {
      error("invalid question update type")
    }

  # check q_ids for length (max 1 league full of questions at a time)
  if(length(q_ids) > 150){
    q_ids <- q_ids[1:150]
  }

  # pull questions
  q_data <- get_question_data(q_ids)

  # write questions and answers to db
  DBI::dbWriteTable(con, "questions", q_data$questions, append = TRUE)
  DBI::dbWriteTable(con, "percents" , q_data$percents , append = TRUE)
  # test <- DBI::dbGetQuery(con,'
  #                    select *
  #                    from "questions"
  #                    ')
  # test2 <- DBI::dbGetQuery(con,'
  #                    select *
  #                    from "percents"
  #                    ')

  DBI::dbDisconnect(con)

  invisible(NULL)
}
