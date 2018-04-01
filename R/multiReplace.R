#' Replace multiple elements in a dataframe at once
#' @param x placeholder
#' @param what placeholder
#' @param by placeholder
#' @export
multiReplace <- function(x, what, by) {
    stopifnot(length(what) == length(by))
    ind <- match(x, what)
    ifelse(is.na(ind), x, by[ind])
}