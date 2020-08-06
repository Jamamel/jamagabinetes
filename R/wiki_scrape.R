library(magrittr)

bj_test <- xml2::read_html('https://es.wikipedia.org/wiki/Anexo:Gabinete_de_Benito_Ju%C3%A1rez')

x <- bj_test %>%
  rvest::html_node('body') %>%
  rvest::html_nodes('li') %>%
  purrr::keep(~ {.x %>%
                rvest::html_children() %>%
                length} == 2) %>%
  purrr::map(~ .x %>%
               rvest::html_children() %>%
               rvest::html_children() %>%
               rvest::html_text() %>%
               data.table::data.table(ministry = .[1],
                                      secretary = .[-1]) %>%
               .[, '.' := NULL]) %>%
  data.table::rbindlist()

x
