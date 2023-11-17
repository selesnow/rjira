#' Get issue worklogs
#'
#' Returns worklogs for an issue, starting from the oldest worklog or from the worklog started on or after a date and time. For more details see next [link](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-worklogs/#api-rest-api-3-issue-issueidorkey-worklog-get).
#'
#' @param issue_id_or_key The ID or key of the issue.
#' @param started_after The worklog start date and time.
#' @param started_before The worklog start date and time.
#' @param expand Use expand to include additional information about worklogs in the response. This parameter acceptsproperties, which returns worklog properties.
#'
#' @return tibble with worklogs
#' @export
jr_get_issues_worklogs <- function(
    issue_id_or_key,
    started_after   = NULL,
    started_before  = NULL,
    expand          = 'properties'
) {

  issues_worklogs <- pbapply::pblapply(
    issue_id_or_key,
    jr_get_issue_worklogs,
    started_after  = started_after,
    started_before = started_before,
    expand         = expand
  )

  result <- bind_rows(issues_worklogs)
  return(result)

}

# helper function
jr_get_issue_worklogs <- function(
    issue_id_or_key,
    started_after   = NULL,
    started_before  = NULL,
    expand          = 'properties'
) {

  # convert to unix timestamp
  if (!is.null(started_after)) {
    started_after  <- as.POSIXct(str_glue('{as.Date(started_after)} 00:00:00')) %>% as.numeric() * 1000
  }
  if (!is.null(started_before)) {
    started_before <- as.POSIXct(str_glue('{as.Date(started_before)} 00:00:00')) %>% as.numeric() * 1000
  }

  # pagination
  start_at <- 0
  total    <- NULL
  result   <- list()

  while ( is.null(total) || start_at <= total ) {

    resp <- jr_make_request(
      path = str_glue('issue/{issue_id_or_key}/worklog'),
      params = list(
        startAt       = start_at,
        startedAfter  = started_after,
        startedBefore = started_before,
        expand        = expand,
        maxResults    = 5000
      )
    )

    worklogs <- tibble(worklogs = list(resp$worklogs)) %>%
      unnest_longer('worklogs') %>%
      unnest_wider('worklogs') %>%
      unnest_wider('author', names_sep = "_") %>%
      rename_with(to_snake_case)

    result <- append(result, list(worklogs))

    start_at <- start_at + 5000

    if ( is.null(total) ) {
      total    <- resp$total
    }

  }

  result <- bind_rows(result)

  return(result)

}
