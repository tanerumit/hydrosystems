#' Potential evapotranspiration (PET)
#'
#' \code{PET_calculate()} returns monthly or daily PET values with the desired
#' calculation approach based on the input arguments
#' \url{https://en.wikipedia.org/wiki/Evapotranspiration}
#'
#' @param date a time-series date object
#' @param tavg a vector of average temperature values (°C)
#' @param tdif a vector of differences computed from maximum and minimum
#' temperatures
#' @param lat latitude information (enter negative values for southern hemisphere)
#' @param method PET calculation method (currently only hargreaves formula is
#' used)
#'
#' @return the output is a vector of PET values
#' @export
#'
PET_calculate <- function(date, tavg, tdif, lat, method = "hargreaves") {

  #Extract years & months from the date object
  years_num <- length(unique(as.numeric(format(date,"%Y"))))
  months    <- as.numeric(format(date,"%m"))

  lookUp <- data.frame(m = 1:12,
    days.m = c(31,28,31,30,31,30,31,31,30,31,30,31),
    days.j = c(15,46,75,106,136,167,197,228,259,289,320,350))

  DaysInMonth <- lookUp$days.m[months]
  JulianDay <- lookUp$days.j[months]

  if(method == "hargreaves") {

    dr = (1+0.033*cos(2*pi/365*JulianDay))
    phi = pi/180*BasinLat
    delta = 0.409*sin((2*pi/365*JulianDay)-1.39)
    ws = acos(-tan(phi)*tan(delta))
    Rs = ((24*60/pi)*0.082*dr*(ws*sin(phi)*sin(delta) +
        cos(phi)*cos(delta)*sin(ws)))*0.408*DaysInMonth
    PET = 0.0023*Rs*(tavg + 17.8)*sqrt(tdif)

  }
}