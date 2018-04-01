#' Create a tibble from All Combinations of Factor Variables
#'
#' @param ... vectors to be passed to the function
#' @export
#' @import dplyr
expandGrid <- function(...) {expand.grid(...)  %>% as_data_frame()}