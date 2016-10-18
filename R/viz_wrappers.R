
#' Add transperancy for the color brewer
#'
#' @param col a vector of colors
#' @param alpha desired alpha level
#'
#' @return
#' @export
#'
#' @examples
brewer_alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  as.vector(apply(sapply(col, col2rgb)/255, 2,
    function(x) rgb(x[1], x[2], x[3], alpha=alpha)))
}


#' Equation Display function for ggplot2
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
lm_eqn = function(df) {
  
  #Equation Display function on the plot
  m = lm(V2~V1, data=df)
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
    list(a = format(coef(m)[1], digits = 2),
      b = format(coef(m)[2], digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}



