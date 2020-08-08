#' Apply transformation from Wiki text to date
#'
#' @param x vector with parsed raw date text
#'
#' @export
date_trans <- function(x){

  dplyr::case_when(

    stringr::str_detect(x, 'enero') ~ stringr::str_replace(x, 'enero', 'january'),
    stringr::str_detect(x, 'febrero') ~ stringr::str_replace(x, 'febrero', 'february'),
    stringr::str_detect(x, 'marzo') ~ stringr::str_replace(x, 'marzo', 'march'),
    stringr::str_detect(x, 'abril') ~ stringr::str_replace(x, 'abril', 'april'),
    stringr::str_detect(x, 'mayo') ~ stringr::str_replace(x, 'mayo', 'may'),
    stringr::str_detect(x, 'junio') ~ stringr::str_replace(x, 'junio', 'june'),
    stringr::str_detect(x, 'julio') ~ stringr::str_replace(x, 'julio', 'july'),
    stringr::str_detect(x, 'agosto') ~ stringr::str_replace(x, 'agosto', 'august'),
    stringr::str_detect(x, 'septiembre') ~ stringr::str_replace(x, 'septiembre', 'september'),
    stringr::str_detect(x, 'octubre') ~ stringr::str_replace(x, 'octubre', 'october'),
    stringr::str_detect(x, 'noviembre') ~ stringr::str_replace(x, 'noviembre', 'november'),
    stringr::str_detect(x, 'diciembre') ~ stringr::str_replace(x, 'diciembre', 'december'),
    TRUE ~ x

  ) %>%
    stringr::str_replace_all('de ', 'of ') %>%
    stringr::str_replace('(?<=[^(of)]) (?=\\d{4})', ' of ') %>%
    lubridate::as_date(format = '%d of %B of %Y')

}
