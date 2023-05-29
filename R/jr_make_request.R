jr_make_request <- function(
  path,
  body = NULL
) {

  req <- request(jr_get_url()) %>%
    req_url_path_append('rest/api/3/') %>%
    req_url_path_append(path) %>%
    req_auth_basic(jr_get_user(), jr_get_token())

  if ( !is.null(body) ) {

    req <- req_body_json(req, body)
  }

  resp <- req_perform(req) %>%
          resp_body_json()

  return(resp)
}
