jr_make_request <- function(
  path,
  body = NULL,
  params = NULL
) {

  req <- request(jr_get_url()) %>%
    req_url_path_append('rest/api/3/') %>%
    req_url_path_append(path) %>%
    req_auth_basic(jr_get_user(), jr_get_token()) %>%
    req_error(body = jr_error_body) %>%
    req_retry(max_tries = 7)

  if ( !is.null(body) ) {

    req <- req_body_json(req, body)

  }

  if ( !is.null(params) ) {

    req <- req_url_query(req, !!!params)

  }

  resp <- req_perform(req) %>%
          resp_body_json()

  return(resp)
}



jr_error_body <- function(resp) {
  resp %>% resp_body_json() %>% purrr::pluck('errorMessages', 1)
}

