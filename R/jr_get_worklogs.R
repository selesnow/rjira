#' Get worklogs
#'
#' Returns worklog details for a list of worklog IDs.
#'
#' @param ids A vector of worklog IDs.
#' @param expand Use expand to include additional information about worklogs in the response. This parameter accepts properties that returns the properties of each worklog.
#'
#' @return tibble with worklog details
#' @export
#'
#' @examples
#' \dontrun{
#' # get worklogs ids updated yesterday
#' updated_wl <- jr_get_updated_worklogs(Sys.Date()-1)
#' # get worklogs details
#' worklogs <- jr_get_worklogs(updated_wl$worklog_id)
#'
#' }
jr_get_worklogs  <- function(
    ids,
    expand = 'properties'
) {

  chunks <- split(ids, (seq_along(ids) - 1) %/% 1000)
  result <- pbapply::pblapply(
    chunks,
    jr_get_worklogs_helper,
    expand = expand
    )
  result <- bind_rows(result)

  return(result)

}

# helper function
jr_get_worklogs_helper <- function(
    ids,
    expand = 'properties'
) {

  if (length(ids)==1) {
    ids <- list(ids)
  }

  resp <- jr_make_request(
    path = str_glue('worklog/list'),
    params = list(
      expand = expand
    ),
    body = list(
      ids = ids
    )
  )

  worklogs <- tibble(worklogs = resp) %>%
    unnest_wider('worklogs') %>%
    unnest_wider('author', names_sep = "_") %>%
    unnest_wider('updateAuthor', names_sep = "_") %>%
    rename_with(to_snake_case)

}
