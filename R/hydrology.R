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
    phi = pi/180*lat
    delta = 0.409*sin((2*pi/365*JulianDay)-1.39)
    ws = acos(-tan(phi)*tan(delta))
    Rs = ((24*60/pi)*0.082*dr*(ws*sin(phi)*sin(delta) +
        cos(phi)*cos(delta)*sin(ws)))*0.408*DaysInMonth
    PET = 0.0023*Rs*(tavg + 17.8)*sqrt(tdif)

  }
}


#' Simulate streamflow using 'ABCD' Hydrology model (no snow)
#'
#' \code{abcd_qest()} returns monthly or daily streamflow time-series
#' @param parm abcd model parameters
#' @param P a vector of precipitation time-series (mm)
#' @param PE a vector of potential evaporation time-series (mm)
#' @param S_ini is the initial soil moisture (mm)
#' @param G_ini is the initial groundwater storage (mm)
#' @return the output is a vector of PET values
#' @export
abcd_qest <- function(parm, P, PE, S_ini , G_ini, print.all = FALSE) {

  #parameters = a vector with a,b,c,d
  #P	= a vector with precip time series for current station
  #PE	= a vector with potential ET time series for current station
  #T	= a vector with tavg time series for current statio
  #Qobs	= a vector with observed streamflow time series for current station
  #Sint = initial soil moisture
  #Gint = initial groundwater storage
  #Aint = initial snow accumulation

  #MODEL PARAMETERS
  #a = runoff & recharge (when soil is under-saturated)    [0-1]
  #b = saturation level                                    [?14 - ?1900]
  #c = ratio of groundwater recharge to surface runoff     [0-1]
  #d = the rate of groundwater discharge                   [0-1]

  #Calibration period length
  final <- length(PE)

  W <- array(0,final)   	 #available water
  Y <- array(0,final)   	 #evapotranspiration opportunity
  S <- array(0,final)  	   #soil moisture
  E <- array(0,final)      #actual evaporation
  G <- array(0,final)  	   #groundwater storage
  Qest <- array(0,final)   #estimated surface runoff

  for (i in 1:final) {

    W[i] <- ifelse(i == 1, P[i] + S_ini, P[i] + S[i-1])

    #w1 and w2 are intermediate values used to calculate Y
    w1 <- (W[i]+parm[2])/(2*parm[1])
    w2 <- W[i]*parm[2]/parm[1]

    Y[i] <- w1 - sqrt((w1^2)-w2)
    S[i] <- Y[i]*exp(-1*PE[i]/parm[2])
    E[i] <- Y[i]*(1-exp(-1*(PE[i]/parm[2])))

    G[i] <- ifelse(i == 1,
      (G_ini + parm[3]*round((W[i]-Y[i]),2))/(1+parm[4]),
      (G[i-1] + parm[3]*round((W[i]-Y[i]),2))/(1+parm[4]))

    Qest[i] <- (1-parm[3])*round((W[i]-Y[i]),2)+parm[4]*G[i]

  }

  if(print.all == FALSE) {
    return(Qest) } else {return(list(Qest, S = S, G = G))}
}

################################################################################

#' Calibrate 'abcd' hydrology model
#'
#' \code{abcd_calibrate()} returns calibration performance measures for abcd model
#' @param ... parameters to be passed to abcd_qest()
#' @param metric performance metric, either 'KGE', 'RMSE' or 'NSE'
#' @param na.rm logical parameter to remove NA values
#' @return the performance measure of the abcd model
#' @export
abcd_calibrate <- function(..., Q.obs, metric = "KGE", na.rm = FALSE) {

  #Estimated streamflow
  Q.est <- abcd_qest(...)

  #Exclude missing data from calibration
  if(na.rm == TRUE) {
    if(any(is.na(Q.obs))) {
      indNA <-  which(is.na(Q.obs))
      Q.obs <- Q.obs[-indNA]
      Q.est <- Q.est[-indNA]}
  }

  #ROOT MEAN SQUARE ERROR (RMSE)
  if(metric == "RMSE") {
    Val <- sqrt(mean((Q.obs - Q.est)^2))}


  #KLING-GUPTA EFFICIENCY (KGE)
  if(metric == "KGE") {
    cc    <- cor(Q.obs, Q.est)
    beta  <- mean(Q.est)/mean(Q.obs)
    alpha <- sd(Q.est)/sd(Q.obs)
    Val   <- 1 - (1 - sqrt((cc-1)^2 + (alpha-1)^2 + (beta-1)^2))
  }

  #NASH SUTCLIFFE EFFICIENCY (NSE)
  if(metric == "NSE") {
    #Nash-Sutcliffe efficiency
    NS1 <- mapply(function(x,y) (x-y)^2, Q.obs, Q.est)
    NS2 <- mapply(function(x,y) (x-y)^2, Q.obs, mean(Q.obs))
    Val <- sum(NS1)/sum(NS2)
  }

  return(Val)

}

