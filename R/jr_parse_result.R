jr_simple_parse <- function(resp) {

  res <- tibble(data = list(resp)) %>%
    unnest_longer(data) %>%
    unnest_wider(data)

  return(res)

}
