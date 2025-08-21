#' Search for issues using JQL
#'
#' @param fields A list of fields to return for each issue, use it to retrieve a subset of fields. This parameter accepts a comma-separated list.
#' @param expand Use expand to include additional information about issues in the response. Note that, unlike the majority of instances where expand is specified, expand is defined as a list of values. The expand options are:
#' @param jql A JQL expression. See [documentation](https://confluence.atlassian.com/x/egORLQ)
#'
#' @details
#' You can see details [this link](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issue-search/#api-rest-api-3-search-jql-post)
#'
#'
#' @return tibble with search result
#' @export
#' @examples
#' \dontrun{
#' issues <- jr_issue_search(jql =
#' '
#'  (updated >= startOfDay("-3d") AND updated <= endOfDay("-3d"))
#'   OR
#'  (created >= startOfDay("-3d") AND created <= endOfDay("-3d"))
#'  '
#' )
#' }
jr_issue_search <- function(
    fields = '*all',
    expand = c('schema', 'names'),
    jql    = ""
) {

  if (length(fields) == 1) {
    fields <- as.list(fields)
  }

  if (length(expand) > 1) {
    expand <- paste0(expand, collapse = ',')
  }

  # pagination
  nextPageToken <- NULL
  isLast        <- FALSE
  result        <- list()

  while ( !isLast ) {

    resp <- jr_make_request(
      path = 'search/jql',
      body = list(
        jql           = jql,
        nextPageToken = nextPageToken,
        maxResults    = 500,
        fields        = fields,
        expand        = expand
      )
    )

    issues <- tibble(issue = list(resp$issues)) %>%
      unnest_longer('issue') %>%
      unnest_wider('issue') %>%
      unnest_wider('fields')

    result <- append(result, list(issues))

    nextPageToken <- resp$nextPageToken
    isLast        <- resp$isLast

  }

  result <- bind_rows(result)

  return(result)

}
