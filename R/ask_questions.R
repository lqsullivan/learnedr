#' Title
#'
#' @param con
#' @param user
#'
#' @return
#' @export
#'
#' @examples
ask_questions <- function(user){
  # get some questions
  con <- DBI::dbConnect(drv    = RSQLite::SQLite(),
                        dbname = "H:/projects/stats_projects/learnedr/learned_league.sqlite")

  # identify previously answered questions
  # TODO: handle missing or incorrect tables better
  if (nrow(DBI::dbGetQuery(conn      = con,
                           statement = "select 'answers'
                                        from sqlite_master
                                        where type='table'
                                        and name='answers'")) != 0) {
    my_answered <-
      DBI::dbGetQuery(conn = con,
                      statement = paste0('select id
                     from "answers"
                     where user =="', user, '"'))
  } else {
    my_answered <- NULL
  }


  # identify unanswered questions
  my_unanswered <-
    DBI::dbGetQuery(conn = con,
                    statement = paste0('select id
                                         from "questions"
                                         where id NOT IN ("',
                                       paste0(my_answered$id,
                                              collapse = '", "'),
                                       '")'))

  q_data <- DBI::dbGetQuery(conn = con,
                            statement = paste0('select *
                                                 from "questions"
                                                 where id IN ("',
                                               paste0(my_unanswered$id,
                                                      collapse = '", "'),
                                               '")')) %>%
    sample_frac(size = 1)

  # initialize answer data
  answer_list <- vector(mode = "list", length = nrow(my_unanswered))

  # while continue
  for (i in 1:nrow(my_unanswered)) {
    # pose question
    cat("\014", q_data$id[i], "\n", q_data$category[i], "\n", q_data$question[i], "\n\n")
    # present supplement (image/sound)
    if (!is.na(q_data$link[i])) {
      if (sub(".*\\.", "", q_data$link[i]) %in% c("jpg", "png", "gif")) {
        # present image
        pic <- jpeg::readJPEG(source = paste0("./question_files/",
                                              sub("/.*/", "", q_data$link[i])),
                              native = TRUE)
        plot(x = 0:1, y = 0:1, type = "n", ann = FALSE, axes = FALSE, asp = dim(pic)[1]/dim(pic)[2])
        rasterImage(pic, 0, 0, 1, 1)
      } else if (sub(".*\\.", "", q_data$link[i]) == "mp3") {
        message("Doesn't currently autoplay sound files")
      }
    }

    answer_start <- Sys.time()
    # prompt answer
    my_ans <- readline(prompt = "Answer: ")
    answer_time <- as.numeric(Sys.time() - answer_start)

    # check exit
    if (my_ans == "") {
      my_ans <- readline(prompt = "Press enter again to quit")

      if (my_ans == "") {
        break
      }
    }

    # check result
    if (tolower(my_ans) == tolower(q_data$answer[i])) {
      correct <- TRUE
    } else {
      # prompt if needed
      cat("\nCORRECT ANSWER: ", q_data$answer[i],
          "\n   YOUR ANSWER: ", my_ans)
      correct <- readline(prompt = "Were you correct (y/n)? ") %in% c("Y", "y", "yes")
    }

    # save my answer
    answer_list[[i]] <- data.frame(id = q_data$id[i],
                                   user = user,
                                   answer = my_ans,
                                   correct = correct,
                                   time = answer_start,
                                   response_time = answer_time)

  }

  # answered questions to db
  answers <- do.call(rbind, answer_list)
  if (!is.null(answers)) {
    DBI::dbWriteTable(con, "answers", answers, append = TRUE)
  }

  invisible(NULL)
}
