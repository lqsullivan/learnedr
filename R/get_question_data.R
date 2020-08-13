#' get_question_data
#'
#' @param q_ids vector of character question ids (format ll##md##q#)
#'
#' @return list with two elements: questions (tibble of question data) and percents (tibble of correct answer %s)
#' @export
#'
#' @examples
get_question_data <- function(q_ids){
  # group by match-day (to attach category)
  match_days <-
    sub("q\\d", "", q_ids)

  md_split <- split(q_ids, match_days)

  # set up progress bar
  prog <- txtProgressBar(min = 0, max = length(q_ids), style = 3)

  # initialize data storage (by match-day)
  q_out   <- vector(mode = "list", length = length(md_split))
  p_out <- vector(mode = "list", length = length(md_split))

  # pull match-day
  for (i in 1:length(md_split)) {
    q_matchday <- vector(mode = "list", length = length(md_split[[i]]))

    p_matchday <- vector(mode = "list", length = length(md_split[[i]]))

    # pull question inside match-day
    for (j in 1:length(md_split[[i]])) {
      id_nums <- extract_id_nums(md_split[[i]][j])

      q_page <- get_question_page(league   = as.integer(id_nums[1]),
                                  day      = as.integer(id_nums[2]),
                                  question = as.integer(id_nums[3]))

      # outputs for 2 tables
      q_matchday[[j]]   <-
        extract_question_data(q_page) %>%
        mutate(id = md_split[[i]][j])

      p_matchday[[j]] <-
        extract_percent_correct(q_page) %>%
        mutate(id = md_split[[i]][j])

      # download linked file if it exists
      if(!is.na(q_matchday[[j]]$link)){
        get_question_file(link = q_matchday[[j]]$link)
      }

      # do this responsibly! it's going to take forever and try to hide its tracks
      Sys.sleep(time = pmax(1, rnorm(1, 3, 1.2)))
      setTxtProgressBar(prog, which(q_ids == md_split[[i]][j]))
    }

    # pull categories
    md_page <- get_matchday_page(league   = as.integer(id_nums[1]),
                                 day      = as.integer(id_nums[2]))
    md_data <- extract_matchday_data(md_page)

    # bind rows and attach categories
    q_out[[i]] <- cbind(do.call(rbind, q_matchday), md_data)
    p_out[[i]] <- do.call(rbind, p_matchday)

    Sys.sleep(time = pmax(1, rnorm(1, 3, 1.2)))
  }

  return(list(questions = do.call(rbind, q_out),
              percents  = do.call(rbind, p_out)))
}
