#' Title
#'
#' @param con database connection
#'
#' @return
#' @export
#'
#' @examples
next_league <- function(con){
  # identify questions in db
  q_in_db <-
    tryCatch(
      expr = {
        DBI::dbGetQuery(con,
                        'select *
                       from "questions"')$id
      },
      error = function(e){
        NA
      }
    )

  max_league <-
    c(51, stringr::str_sub(q_in_db, 3, 4) %>% as.integer()) %>%
    sort(decreasing = TRUE) %>%
    `[`(1)

  q_ids <-
    expand.grid(ll = max_league + 1,
                md = 1:25,
                q  = 1:6) %>%
    mutate(md_0 = ifelse(nchar(md) == 1, paste0("0", md), md)) %>%
    rowwise() %>%
    mutate(id = paste0("ll", ll, "md", md_0, "q", q))

  return(q_ids$id)
}
