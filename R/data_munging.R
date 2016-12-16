
#' Create a tibble from All Combinations of Factor Variables
#'
#' @param ... vectors to be passed to the function
#' @export
#' @import dplyr
expandGrid <- function(...) {expand.grid(...)  %>% as_data_frame()}


#-------------------------------------------------------------------------------
#
#' Create a tibble from the Combinations of data frames
#'
#' @param ... data frames to be passed to the function
#' @export
#' @import dplyr
expandGridDf <- function(...) {
  Reduce(function(...) merge(..., by = NULL), list(...)) %>% as_data_frame()
}

#-------------------------------------------------------------------------------

#' Interpolation from 2-D matrix
#'
#' @param x parameter on x-axis
#' @param y parameter on y-axis
#' @param z parameter on z-axis
#' @param resolution resolution used for interpolating z
#' @export
gridInterpolate <- function(x, y, z = NULL, resolution = 100, ...) {

  # Interpolation for three-dimensional array
  if (is.null(z)) {z <- rep(0, length(x))}

  z <- data.frame(z)

  df1 <- lapply(seq_len(ncol(z)), function(i) akima::interp(x, y, z[, i],
      xo = seq(min(x), max(x), length = resolution),
      yo = seq(min(y), max(y), length = resolution)), ...)

    df2 <- do.call("cbind", lapply(df1, function(x) c(x$z)))
    df3 <- cbind(expand.grid(x = df1[[1]]$x, y = df1[[1]]$y), df2)

}

#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

#' Rename data frame columns in a list of data frames
#' @param data_list placeholder
#' @param new_names placeholder
#' @export
#'
renameColumns <- function(data_list, new_names) {

    lapply(seq(data_list), function(x) {
        y <- data.frame(LCE_metrics[[x]])
        names(y) <- new_names
        return(y)

    })

}

#-------------------------------------------------------------------------------
#' Find mid-points of equal-length bins
#' @param x placeholder
#' @export
#'
binCentered <- function(x) {
    return(x[-length(x)] + (x[2] - x[1])/2)
}

#-------------------------------------------------------------------------------

