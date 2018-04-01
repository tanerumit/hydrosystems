#' Water Year function
#'
#' \url{http://www.civil.uwaterloo.ca/watflood/manual/02_03_2.htm}
#'
#' @param dates time-series of dates as a date object
#' @param start_month the first month of the water year
#' @return the output is the calculated water year
#' @export
getWaterYear <- function(dates, start_month = 9) {
    dates.posix = as.POSIXlt(dates)
    offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
    return(dates.posix$year + 1900 + offset)
}
