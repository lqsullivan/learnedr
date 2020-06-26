#' Title
#'
#' @param q_ids vector of character question ids (format ll##md##q#)
#' @param cavalier bool indicating whether to pull questions with a kind delay
#'
#' @return list with two elements: questions (tibble of question data) and percents (tibble of correct answer %s)
#' @export
#'
#' @examples
get_question_data <- function(q_ids, cavalier = FALSE){
  q_out <- vector(mode = "list", length = length(q_ids))

  pct_out <- vector(mode = "list", length = length(q_ids))

  prog <- txtProgressBar(min = 0, max = length(q_ids), style = 3
  )
  for(i in 1:length(q_ids)){
    id_nums <- extract_id_nums(q_ids[i])
    page <- get_page(league   = as.integer(id_nums[1]),
                     day      = as.integer(id_nums[2]),
                     question = as.integer(id_nums[3]))

    # outputs for 2 tables
    q_out[[i]]   <-
      extract_question_data(page) %>%
      mutate(id = q_ids[i])

    pct_out[[i]] <-
      extract_percent_correct(page) %>%
      mutate(id = q_ids[i])

    # download linked file if it exists
    if(!is.na(q_out[[i]]$link)){
      get_question_file(link = q_out[[i]]$link)
    }

    if(cavalier){
      warning("You have requested to pull questions in cavalier mode.\nThis will grab from the server with no delay. It's not good resource stewardship and might get your IP blacklisted.")

      still_do <- readline("Are you sure you want to continue in cavalier mode?: ")
      if(!tolower(still_do) %in% c("shut up, nerd")){
        stop("Good choice. Exiting.")
      }
    }

    # do this responsibly! it's going to take forever and try to hide its tracks
    scrape_delay <-
      if(rbinom(1, 1, 0.5)){
        # typically around 5 seconds
        max(1.5, rnorm(1, 11, 2)/2)
      } else{
        5 + rbeta(1, 2, 4) * 11
      }
    setTxtProgressBar(prog, i)
    Sys.sleep(time = scrape_delay)
  }

  return(list(questions = do.call(rbind, q_out),
              percents  = do.call(rbind, pct_out)))
}
