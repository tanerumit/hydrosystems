#' Annualize a time-sequence of costs
#'
#' @param data placeholder
#' @param r discount rate
#' @export
ANNUALIZE <- function(data,r=0.05) {

  #Calculate annulized cost from a sequence of annual costs
  #data = time-series of annual benefits
  #r = discount rate (0.001 - 1)

  PV <- sum(sapply(1:length(data), function(x) data[x]/((1+r)^x)))
  return(PV*(r*(1+r)^length(data))/((1+r)^length(data)-1))
}


#' Calculate the PV from annual cash flows
#'
#' @param data placeholder
#' @param r discount rate
#' @export
PV_CALCULATE <- function(data,r=0.05) {
  sum(sapply(1:length(data), function(x) data[x]/((1+r)^x)))
}


#' Annualized costs from present value
#'
#' @param PV present value cost
#' @param r discount rate
#' @param T_years time horizon
#' @export
ANNUALIZE_FROM_PV <- function(PV,r=0.05,T_years) {
  PV*(r*(1+r)^T_years)/((1+r)^T_years-1)
}


#' present value crom annualized costs
#'
#' @param PV present value cost
#' @param r discount rate
#' @param T_years time horizon
#' @export
PV_FROM_ANNUALIZE <- function(ANNUALIZE,r=0.05,T_years) {
  ANNUALIZE/((r*(1+r)^T_years)/((1+r)^T_years-1))
}

