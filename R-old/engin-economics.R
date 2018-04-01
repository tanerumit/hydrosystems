
#' Calculate the PV from annual cash flows
#'
#' @param data annual stream of benefits or costs
#' @param r discount rate 
#' @export
#' @return returns prevesent value
presentValue <- function(data, r = 0.05) {
    df <- sapply(1:length(data), function(x) data[x]/((1 + r)^x))
    return(sum(df))
}

#' Annualized costs from present value
#'
#' @param data present value term
#' @param r discount rate
#' @export
#' @return annualized return of investment
annualizedReturn <- function(data, r = 0.05) {
    return(data * (r * (1 + r)^length(data))/((1 + r)^length(data) - 1))
}
