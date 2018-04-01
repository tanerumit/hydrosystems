#' Find mid-points of equal-length bins
#' @param x placeholder
#' @export
#'
binCentered <- function(x) {
    return(x[-length(x)] + (x[2] - x[1])/2)
}