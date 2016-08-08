#' Generate a grid output of prim_boxes
#'
#' \code{prim_grid()} returns a data table of prim_boxes with given features of
#' mass, density, coverage, and weighted coverage
#' @param input a matrix of input parameter values
#' @param y is a vector of response values
#' @param minimum risk threshold required for the prim boxes
#' @param vdim is number of dimensions to explore in the input matrix
#' @export
prim_grid <- function(input, y, threshold = 0, vdim = 3, ...) {

  require(prim)
  require(dplyr)

  #All combinations of input variables
  var_com <- t(combn(x = 1:ncol(input), vdim))
  num_com <- nrow(var_com)

  #Store results in a list
  out <- as.list(rep(NA,num_com))

  #Loop through each combination
  for (i in seq_len(num_com)) {

    var_names <- paste(colnames(input)[var_com[i,]], collapse = "-")
    x <- input[,var_com[i,]]

    model_prim <- prim.box(x=x, y=y, ...)

    #Template to store data
    num <- model_prim$num.hdr.class
    df <- data_frame(vars = 0, Boxes = seq_len(num),
      fun = 0, mass = 0, dens = 0, cov = 0, wcov = 0)

    #Points in the box
    points_box <- sapply(1:num, function(x) model_prim$y[[x]])
    points_all <- y

    #Points of interest in the box
    cond_box <- sapply(1:num, function(x) points_box[[x]][points_box[[x]] > threshold])
    cond_all <- points_all[points_all > threshold]

    #Scenario features
    df$vars <- var_names
    df$fun <- model_prim$y.fun[1:num]
    df$mass <- sapply(1:num, function(x) length(points_box[[x]])/length(points_all))
    df$dens <- sapply(1:num, function(x) length(cond_box[[x]])/length(points_box[[x]]))
    df$cov  <- sapply(1:num, function(x) length(cond_box[[x]])/length(cond_all))
    df$wcov <- sapply(1:num, function(x) sum(cond_box[[x]])/sum(cond_all))

    out[[i]] <- df
  }

  #return output
  return(bind_rows(out))
}