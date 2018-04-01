#'  ABCD conceptual rainfall - runoff simulation model
#'
#' \code{abcd_qest()} returns monthly or daily streamflow time-series
#'  The ABCD water balance model is a simple hydrologic model for simulating
#'  streamflow in response to precipitation and potential evapotranspiration
#'  developed by Thomas (1981). The model is comprised of two storage
#'  compartmens: soil moisture and groundwater. The soil moisture gains water
#'  from precipitation and loses water to evapotranspiration (ET), surface
#'  runoff and groundwater recharge. The groundwater compartment gains water
#'  from recharge and loses water as discharge. The total streamflow is the sum
#'  of surface runoff from the soil moisture and groundwater discharge.
#'  The model has four parameters: a, b, c, a, d.
#' \itemize{
#' \item a ranges between 0 and 1.
#' \item b ranges between 260 - 1900 (Vandewiele et al. 1992).
#' \item c ranges between 0 - 1.
#' \item d ranges between 0 - 1.
#' }
#' @param parm  abcd model parameters
#' @param P     a vector of precipitation time-series (mm)
#' @param PE    a vector of potential evaporation time-series (mm)
#' @param S_ini is the initial soil moisture (mm)
#' @param G_ini is the initial groundwater storage (mm)
#' @return      the output is a time-series of run-off values
#' @export
abcdQest <- function(parm, P, PE, S_ini, G_ini, print.all = FALSE) {

    # parameters = a vector with a,b,c,d P = a vector with precip time series for
    # current station PE = a vector with potential ET time series for current
    # station T = a vector with tavg time series for current statio Qobs = a
    # vector with observed streamflow time series for current station Sint =
    # initial soil moisture Gint = initial groundwater storage Aint = initial snow
    # accumulation

    # MODEL PARAMETERS a = runoff & recharge (when soil is under-saturated) [0-1]
    # b = saturation level [?14 - ?1900] c = ratio of groundwater recharge to
    # surface runoff [0-1] d = the rate of groundwater discharge [0-1]

    # Calibration period length
    final <- length(PE)

    W <- array(0, final)  #available water
    Y <- array(0, final)  #evapotranspiration opportunity
    S <- array(0, final)  #soil moisture
    E <- array(0, final)  #actual evaporation
    G <- array(0, final)  #groundwater storage
    Qest <- array(0, final)  #estimated surface runoff

    for (i in 1:final) {

        W[i] <- ifelse(i == 1, P[i] + S_ini, P[i] + S[i - 1])

        # w1 and w2 are intermediate values used to calculate Y
        w1 <- (W[i] + parm[2])/(2 * parm[1])
        w2 <- W[i] * parm[2]/parm[1]

        Y[i] <- w1 - sqrt((w1^2) - w2)
        S[i] <- Y[i] * exp(-1 * PE[i]/parm[2])
        E[i] <- Y[i] * (1 - exp(-1 * (PE[i]/parm[2])))

        G[i] <- ifelse(i == 1, (G_ini + parm[3] * round((W[i] - Y[i]), 2))/(1 +
            parm[4]), (G[i - 1] + parm[3] * round((W[i] - Y[i]), 2))/(1 + parm[4]))

        Qest[i] <- (1 - parm[3]) * round((W[i] - Y[i]), 2) + parm[4] * G[i]

    }

    if (print.all == FALSE) {
        return(Qest)
    } else {
        return(list(Qest, S = S, G = G))
    }
}