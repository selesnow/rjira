jr_authorize <- function(
  url,
  user,
  token
) {

  jr_set_url(url)
  jr_set_token(token)
  jr_set_user(user)
  cli::cli_alert_success('Jira authorize success')

}

jr_set_url <- function(url) {
  Sys.setenv('JIRA_URL' = url)
  Sys.setenv('JIRA_BASE_API_URL' = stringr::str_glue('{url}/rest/api/3/'))
  cli::cli_alert_info('Set Jira URL')
}

jr_set_user <- function(user) {
  Sys.setenv('JIRA_USER' = user)
  cli::cli_alert_info('Set Jira user')
}

jr_set_token <- function(token) {
  Sys.setenv('JIRA_TOKEN' = token)
  cli::cli_alert_info('Set Jira API token')
}

jr_get_token <- function() {
  Sys.getenv('JIRA_TOKEN')
}

jr_get_user <- function() {
  Sys.getenv('JIRA_USER')
}

jr_get_url <- function() {
  Sys.getenv('JIRA_URL')
}

jr_get_base_url <- function() {
  Sys.getenv('JIRA_BASE_API_URL')
}

jr_check_auth <- function() {

  if (jr_get_token() != ""||jr_get_user != ""||jr_get_base_url != "") {
    return(TRUE)
  } else {
    stop("You need authorize, please use jr_authorize() function, after repeat this call")
  }

}
