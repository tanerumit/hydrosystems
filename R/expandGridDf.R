#
#' Create a tibble from the Combinations of data frames
#'
#' @param ... data frames to be passed to the function
#' @export
#' @import dplyr
expandGridDf <- function(...) {
  Reduce(function(...) merge(..., by = NULL), list(...)) %>% as_data_frame()
}