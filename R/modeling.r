
#' Calculate goodness of fit parameters
#'
#' @param observed observed results
#' @param simulated simulation results
#' @param output measure to be calculated: NSE, KGE, RMSE or bias
#' @export
goodnessOfFit <- function(observed, simulated, output = "NSE") {

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
piecewiseLm <- function(var_name, knots) {
  formula.sign <- rep(" - ", length(knots))
  formula.sign[knots < 0] <- " + "
  paste(var.name, "+",
    paste("I(pmax(", var_name, formula.sign, abs(knots), ", 0))",
      collapse = " + ", sep=""))
}


#' K nearest neighbors (KNN) bootrapping
#'
#' \code{knn()} returns a sample based on knn scheme
#' @param x is the prior value selected
#' @param k is the number of nearest points
#' @param s is the vector to sample from
#' @export
kNearestNeigboors <- function(x, k, s) {
  
  x.dis <- sqrt((s - x)^2)
  s.ind <- which(x.dis %in% sort(x.dis)[1:k])
  s.wgh <- sapply(1:k, function(y) (1/y)/(sum(1/(1:k))))
  x.new <- sample(s[s.ind], size = 1, replace = TRUE, prob = s.wgh)
  x.new.ind <- which(x.new == s)
  
  return(x.new.ind)
}

#' Inverse Box-cox transformation
#'
#' \code{boxcox_inverse()} returns a sample based on knn scheme
#' @param lamda value extracted from power transform
#' @param Y box-cox transformation value
#' @export
boxcoxTranformInverse <- function(lambda, Y) {
  
  if (lambda == 0) {
    result <- exp(Y)
  }
  if (lambda != 0) {
    result <- (lambda * Y + 1)^(1/lambda)
  }
  return(result)
  
}


#' Function to extract the overall ANOVA p-value out of a linear model object
#'
#' \code{extract_pval()} returns a sample based on knn scheme
#' @param modelobject is the model to extract p values from
#' @export
extractPvalue <- function(modelobject) {
  if (class(modelobject) != "lm") 
    stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1], f[2], f[3], lower.tail = F)
  attributes(p) <- NULL
  return(p)
}


