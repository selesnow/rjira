#' Get IDs of updated worklogs
#'
#' Returns a list of IDs and update timestamps for worklogs updated after a date and time.
#' For more information see next [link](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-worklogs/#api-rest-api-3-worklog-updated-get).
#'
#' @param since The date and time, after which updated worklogs are returned.
#' @param expand Use expand to include additional information about worklogs in the response. This parameter accepts properties that returns the properties of each worklog.
#'
#' @return tibble with IDs and update timestamps for worklogs
#' @export
#' @examples
#' \dontrun{
#' # get worklogs ids updated yesterday
#' updated_wl <- jr_get_updated_worklogs(Sys.Date()-1)
#' }
jr_get_updated_worklogs <- function(
    since,
    expand = 'properties'
) {


  # convert to unix timestamp
  if (!is.null(since)) {
    since  <- as.POSIXct(since) %>% as.numeric() * 1000
  }

  # pagination
  lastPage   <- FALSE
  result     <- list()

  while ( !lastPage ) {

    resp <- jr_make_request(
      path = str_glue('worklog/updated'),
      params = list(
        since         = since,
        expand        = expand
      )
    )

    worklogs <- tibble(worklogs = list(resp$values)) %>%
      unnest_longer('worklogs') %>%
      unnest_wider('worklogs') %>%
      rename_with(to_snake_case)

    result <- append(result, list(worklogs))

    lastPage <- resp$lastPage
    if (!lastPage) {
      since    <- urltools::param_get(resp$nextPage, 'since')$since %>% as.numeric()
    }
  }

  result <- bind_rows(result)

  return(result)

}
