#' Reservoir simulation model
#'
#' \code{reservoir_sim()} returns simulation results from a reservoir analysis
#'
#' @param  beg.y        First year of the simulation period [dimless]
#' @param  beg.m        First month of the simulation period [dimless]
#' @param  K,           Reservoir design storage capacity [MCM]
#' @param  K_dead,      top of inactive storage volume [MCM]
#' @param  pool_flood   relative size of flood protection pool
#' @param  pool_cons    relative size of conservation pool
#' @param  pool_buff    relative size of buffer pool
#' @param  buffer       reservoir storage buffer coefficient [dimless]
#' @param  Q,           Monthly inflows to reservoir [MCM/month]
#' @param  T_dom,       Domestic target release [MCM]
#' @param  T_irr,       Irrigation target release [MCM]
#' @param  T_eco,       Environmental target release [MCM]
#' @param  evap.m       per area evaporation from reservoir [MCM/month]
#' @param  f_elev       evalation as f(volume) [approxfun obj]
#' @param  f_vol        volume as f(elevation) [approxfun obj]
#' @param  f_sarea      s.area as f(volume) [approxfun obj]
#' @param  S_fr         initial fill ratio at the first month [dimless]
#' @param  cycle        time-series double cycling [T/F]
#' @param  C_alloc      allocation coefficient [dimless]
#' @export
reservoirSimulation <- function(
  beg.y = 2020, beg.m = 1, K, K_dead, pool_flood = 0, pool_cons  = 0.7,
  pool_buff  = 0.3, buffer = 1, Q, T_dom, T_irr, T_eco, evap.m,
  f_elev, f_vol, f_sarea, S_fr = 0.8, cycle = FALSE,  C_alloc = 0.8,
  priority = c("T_eco","T_dom","T_irr"))
{

  #Packages
  #require(lubridate)
  #require(dplyr)

  #Simulation period, based on the beginning year and ending year
  begin <- as.Date(paste(beg.y, beg.m,"01",sep="-"))
  Date  <- begin + c(0:(length(Q)-1)) * months(1)

  #If a demand target is not defined, set it to zero
  if(!exists(x = "T_dom"))  {T_dom  <- 0}
  if(!exists(x = "T_irr"))  {T_irr  <- 0}
  if(!exists(x = "T_eco"))  {T_eco  <- 0}

  #Convert inputs to monthly time-series
  mon_cf <- as.numeric(days_in_month(1:12)/365)
  if(length(T_irr)  == 1)  T_irr  <- (T_irr * mon_cf)[month(Date)]
  if(length(T_dom)  == 1)  T_dom  <- (T_dom * mon_cf)[month(Date)]
  if(length(T_eco)  == 1)  T_eco  <- (T_eco * mon_cf)[month(Date)]
  if(length(K_dead) == 1)  K_dead <- rep(K_dead, length(Date))

  #Convert inputs to monthly time-series
  if(length(T_irr) == 12) T_irr <- T_irr[month(Date)]
  if(length(T_dom) == 12) T_dom <- T_dom[month(Date)]
  if(length(T_eco) == 12) T_eco <- T_eco[month(Date)]
  if(length(evap.m) == 12) evap.m <- evap.m[month(Date)]

  # Double-cycling of demand time-series
  if (cycle) {
    Q <- rep(Q, 2)
    T_irr <- rep(T_irr, 2)
    T_dom <- rep(T_dom, 2)
    T_eco <- rep(T_eco, 2)
    K_dead  <- rep(K_dead, 2)
    evap.m  <- rep(evap.m, 2)
    Date <- begin + c(0:(length(Q)-1)) * months(1)
  }

  # Define mass balance variables
  pool <- vector("numeric", length=length(Date))
  S    <- vector("numeric", length=length(Date))
  L    <- vector("numeric", length=length(Date))
  Spl  <- vector("numeric", length=length(Date))
  R    <- rep(list(S), length(priority))

  #Simulate mass balance from period 1 to length(Date)
  for (i in 1:length(Date)) {

    #browser()

    #(i == 60) {browser()}

    # current year & month
    m <- month(Date)[i]; y <- year(Date)[i]

    # Reservoir storage at the beginning of period i
    if(i == 1) {S_i <- K * S_fr} else {S_i <- S[i-1]}

    #Storage - Net-evap adjustment
    elev <- f_elev(S_i) - evap.m[i]

    #Storage available for operation
    S[i] <- f_vol(elev) + Q[i]

    #Evaporative losses
    L[i] <- S_i + Q[i] - S[i]

    #Calculate storage levels
    K_active <- K - K_dead[i]
    K_buff <- K_dead[i] + K_active * pool_buff
    K_cons <- K_buff + K_active * pool_cons

    #Storage pools
    S_cons  <- max(S[i] - K_buff, 0)
    S_buff <- if(S[i]>K_buff) {K_buff-K_dead[i]} else {max(S[i]-K_dead[i], 0)}
    pool[i] <- S_cons + S_buff * buffer

    #### Allocation rules +++++++++++++++++++++++++++++++++++++++++
    #1st iteration
    poolc <- pool[i]
    prio1_1 <- min(get(priority[[1]])[i]*C_alloc, poolc)
    poolc <- poolc - prio1_1
    prio2_1 <- min(get(priority[[2]])[i]*C_alloc, poolc)
    poolc <- poolc - prio2_1
    prio3_1 <- min(get(priority[[3]])[i]*C_alloc, poolc)
    poolc <- poolc - prio3_1

    #2nd iteration
    prio1_2 <- min(poolc, get(priority[[1]])[i] - prio1_1)
    poolc <- poolc - prio1_2
    prio2_2 <- min(poolc, get(priority[[2]])[i] - prio2_1)
    poolc <- poolc - prio2_2
    prio3_2 <- min(poolc, get(priority[[3]])[i] - prio3_1)

    R[[1]][i] <- prio1_1 + prio1_2
    R[[2]][i] <- prio2_1 + prio2_2
    R[[3]][i] <- prio3_1 + prio3_2

    #### +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    #Subtract all releases from the intermediate storage volume computed
    S[i] <- S[i] - do.call(sum, lapply(R, "[[", i))

    #Check if there is a spill, and adjust the storage accordingly
    Spl[i] <- max(S[i] - K, 0)
    if(Spl[i] != 0) {S[i] <- S[i] - Spl[i]}

  }

  #Save results to data_frame
  R_dom  <- R[[which(priority == "T_dom")]]
  R_irr  <- R[[which(priority == "T_irr")]]
  R_eco  <- R[[which(priority == "T_eco")]]

  df <- data_frame(Year = year(Date), Month = month(Date), pool,
    S, Q, L, Spl, R_eco, T_eco, R_irr, T_irr, R_dom, T_dom)

  return(df)
}





