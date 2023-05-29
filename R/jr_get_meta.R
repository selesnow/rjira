#' Get create issue metadata
#'
#' @return table with metadata
#' @export
#' @details
#' See [documents](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-issues/#api-rest-api-3-issue-createmeta-get)
#'
jr_get_createmeta <- function() {

  jr_check_auth()

  res <- jr_make_request('issue/createmeta') %>%
         .$projects %>%
         jr_simple_parse()

  return(res)

}
