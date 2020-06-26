#' Title
#'
#' @param con database connection
#'
#' @return
#' @export
#'
#' @examples
fill_gaps <- function(con){
  # identify questions in db
  q_in_db <-
    tryCatch(
      expr = {
        DBI::dbGetQuery(con,
                        'select *
                       from "questions"')
      },
      error = function(e){
        NA
      }
    )

  # find holes or endpoint
  max_id <-
    # if the db is empty, this should take care of it
    c("ll52md25q6", q_in_db) %>%
    sort(decreasing = TRUE) %>%
    `[`(1)
  max_id_nums <-
    max_id %>%
    extract_id_nums()

  # generate all ids between min and max
  all_q <-
    # add 1 for next league
    expand.grid(ll = 52:(as.numeric(max_id_nums[1]) + 1),
                md = 1:25,
                q  = 1:6) %>%
    mutate(md_0 = ifelse(nchar(md) == 1, paste0("0", md), md)) %>%
    rowwise() %>%
    mutate(id = paste0("ll", ll, "md", md_0, "q", q)) %>%
    # not ideal to compare strings like this but it should be fine with the format
    filter(id <= max_id)

  # identify missing ids
  missing_ids <- all_q$id[!all_q$id %in% q_ids]

  return(missing_ids)
}
