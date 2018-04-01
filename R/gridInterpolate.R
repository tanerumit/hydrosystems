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