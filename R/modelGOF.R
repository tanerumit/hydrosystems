#' Calculate goodness of fit parameters
#'
#' @param observed observed results
#' @param simulated simulation results
#' @param output measure to be calculated: NSE, KGE, RMSE or bias
#' @export
modelGOF <- function(observed, simulated, output = "NSE") {

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