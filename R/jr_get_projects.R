#' Get all projects
#'
#' @return tibble with projects info
#' @export
#' @details
#' See [documentation](https://developer.atlassian.com/cloud/jira/platform/rest/v3/api-group-projects/#api-rest-api-3-project-get)
#'
#'
jr_get_projects <- function() {

  jr_check_auth()

  res <- jr_make_request('project') %>%
    jr_simple_parse()

  return(res)

}
