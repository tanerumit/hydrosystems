#' Calculate goodness of fit parameters
#'
#' @param observed observed results
#' @param simulated simulation results
#' @param output measure to be calculated: NSE, KGE, RMSE or bias
#' @export
GoodnessOfFit <- function(observed, simulated, output = "NSE") {

  #Nash-Sutcliffe efficiency (NSE)
  NS1 <- mapply(function(x,y) (x-y)^2, observed, simulated)
  NS2 <- mapply(function(x,y) (x-y)^2, observed, mean(observed))
  NSE <- round(1 - (sum(NS1)/sum(NS2)),3)

  #Root Mean Square Error (RMSE)
  RMSE <- sqrt(sum(NS1)/length(observed))/mean(observed)

  #Model Bias
  bias <- mean((simulated-observed)/observed)

  #Kling-Gupta Efficiency (KGE)
  cc    <- cor(observed, simulated)
  beta  <- mean(simulated)/mean(observed)
  alpha <- sd(simulated)/sd(observed)
  KGE   <- 1 - sqrt((cc-1)^2 + (alpha-1)^2 + (beta-1)^2)

  return(
    switch(output, "NSE" = NSE, "KGE" = KGE, "RMSE" = RMSE, "bias" = bias)
  )

}

#' Piecewise linear regression
#'
#' Estimating continuous piecewise linear regression
#'\url{https://www.r-bloggers.com/estimating-continuous-piecewise-linear-regression/}  
#'
# 'Assume you are given continuous predictor x and continuous predicted variable y. 
#' We want to estimate continuous piecewise linear regression with fixed knots stored 
#' in variable knots using standard lm procedure.
#' The key to a solution is proper definition of regression formula. In order to introduce #' possibility of change of slope in knot k we have to add a so called hinge term to the 
#' model max(0, x-k).
#' In the code given below function piece.formula automatically generates a proper right 
#' hand side of the regression formula given variable name and list of required knots. It #' is next tested on a simple function.

#' @param var_name variable name
#' @param knots position of knots
#' 
#' 
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
piecewise_lm <- function(var_name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
    paste("I(pmax(", var_name, formula.sign, abs(knots), ", 0))",
      collapse = " + ", sep=""))
}

