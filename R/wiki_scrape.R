# library(magrittr)
#' Scrape Wiki page for cabinet data and produce a tidy data.table for analysis
#'
#' @param url url to wiki page from which to extract cabinet information
#'
#' @examples
#' url <- xml2::read_html('https://es.wikipedia.org/wiki/Anexo:Gabinete_de_Benito_Ju%C3%A1rez')
#' parsed_dt <- scrape_cabinet(url)
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
    .[, 'end_year' := lubridate::year(end)] %>%
    .[, 'end_month' := lubridate::month(end)] %>%
    .[, 'start' := ifelse(nchar(start) <= 2,
                          glue::glue('{start} de {dplyr::case_when(

                                                  .$end_month == 1 ~ "enero",
                                                  .$end_month == 2 ~ "febrero",
                                                  .$end_month == 3 ~ "marzo",
                                                  .$end_month == 4 ~ "abril",
                                                  .$end_month == 5 ~ "mayo",
                                                  .$end_month == 6 ~ "junio",
                                                  .$end_month == 7 ~ "julio",
                                                  .$end_month == 8 ~ "agosto",
                                                  .$end_month == 9 ~ "septiembre",
                                                  .$end_month == 10 ~ "octubre",
                                                  .$end_month == 11 ~ "noviembre",
                                                  .$end_month == 12 ~ "diciembre",
                                       )}'),
                          start)] %>%
    .[, 'start' := ifelse(stringr::str_detect(start, '\\d{4}'),
                          start,
                          glue::glue('{start} de {.$end_year}')) %>%
        date_trans()] %>%
    .[, 'start_year' := lubridate::year(start)] %>%
    .[, 'start_month' := lubridate::month(start)] %>%
    .[, c('name', 'note') := entry %>%
        stringr::str_extract('.*(?=\\(\\d+)') %>%
        data.table::tstrsplit(.,
                              split = '\\(')] %>%
    .[, 'note' := note %>%
        stringr::str_replace('\\)','') %>%
        stringr::str_trim()] %>%
    .[, c('timeframe', 'entry') := NULL]

}

