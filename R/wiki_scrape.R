# library(magrittr)
#' Scrape Wiki page for cabinet data and produce a tidy data.table for analysis
#'
#' @param url url to wiki page from which to extract cabinet information
#'
#' @export
scrape_cabinet <- function(url){

  url %>%
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
                                        entry = .[-1]) %>%
                 .[, '.' := NULL]) %>%
    data.table::rbindlist() %>%
    .[, 'entry' := stringr::str_replace_all(entry, '\\}', '')] %>%
    .[, 'timeframe' := entry %>%
        stringr::str_extract('\\(\\d{1,2}.*\\d{4}\\)$') %>%
        stringr::str_replace_all('[\\(\\)]', '')] %>%
    .[, c('start', 'end') := data.table::tstrsplit(timeframe,
                                                   split = ' (al|y) ')] %>%
    .[, 'end' := end %>% date_trans()] %>%
    .[, 'start' := start]


}


# bj_test <- xml2::read_html('https://es.wikipedia.org/wiki/Anexo:Gabinete_de_Benito_Ju%C3%A1rez')


