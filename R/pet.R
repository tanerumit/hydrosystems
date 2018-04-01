#' Potential evapotranspiration (PET) by hargreaves method
#'
#' \url{http://www.civil.uwaterloo.ca/watflood/manual/02_03_2.htm}
#'
#' @param date a time-series date object
#' @param tavg a vector of average temperature values (°C)
#' @param tdif a vector of differences computed from maximum and minimum
#' temperatures
#' @param lat latitude information (negative values for southern hemisphere)

#' @return the output is a vector of PET values
#' @export
hargreavesPET <- function(date, tavg, tdif, lat) {
  
  # Extract years & months from the date object
  years_num <- length(unique(as.numeric(format(date, "%Y"))))
  months <- as.numeric(format(date, "%m"))
  
  lookUp <- data.frame(m = 1:12, 
    days.m = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 
    days.j = c(15, 46, 75, 106, 136, 167, 197, 228, 259,
      289, 320, 350))
  
  DaysInMonth <- lookUp$days.m[months]
  JulianDay <- lookUp$days.j[months]
  
  dr = (1 + 0.033 * cos(2 * pi/365 * JulianDay))
  phi = pi/180 * lat
  delta = 0.409 * sin((2 * pi/365 * JulianDay) - 1.39)
  ws = acos(-tan(phi) * tan(delta))
  Rs = ((24 * 60/pi) * 0.082 * dr * (ws * sin(phi) * sin(delta) + cos(phi) *
      cos(delta) * sin(ws))) * 0.408 * DaysInMonth
  PET = 0.0023 * Rs * (tavg + 17.8) * sqrt(tdif)

  return(PET)
}

#' Potential evapotranspiration (PET) by hamon method
#'
#' \url{http://data.snap.uaf.edu/data/Base/AK_2km/PET/Hamon_PET_equations.pdf}
#'
#' @param date a time-series date object
#' @param tavg average monthly temperature (DegC)
#' @param Ld length of day as multiple of 12 hours (for hamon equation)
#' @param KPEC calibration parameter (for hamon equation)
#' 
#' @return returns monthly PET values (mm/month)
#' @export
hamonPET <- function(tavg, Ld, KPEC) {
  
  #Saturated vapor  pressure (mb) at the given Temp. (Deg C)
  ESAT <- 6.108 * exp(17.26939 * tavg / (tavg + 273.3))
  
  #Saturated vapor density (g/m3) at the daily mean air temparture (T)
  RHOSAT <- 216.7 * ESAT / (tavg + 273.3)
  
  #Daily PET (mm/day)
  PET_daily <- 0.1651 * Ld * RHOSAT * KPEC

  return(PET_daily)
}
