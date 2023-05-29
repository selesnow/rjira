#' Get issue fields
#'
#' @return tibble with issue fields
#' @export
#'
jr_get_fields <- function() {

  jr_check_auth()

  res <- jr_make_request('field') %>%
         jr_simple_parse()

  return(res)

}
