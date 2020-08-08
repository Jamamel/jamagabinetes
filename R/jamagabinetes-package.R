#' @keywords internal
#'
#' @import data.table
#' @import magrittr
#' @importFrom dplyr case_when
#' @importFrom stringr str_replace str_replace_all
#' @importFrom lubridate as_date
#' @importFrom rvest html_node html_nodes
#' @importFrom purrr map
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
globalVariables(c('start', 'end', 'entry', 'timeframe', '.'))
NULL
