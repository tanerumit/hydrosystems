#' Calibrate 'abcd' hydrology model
#'
#' \code{abcd_calibrate()} returns calibration performance measures for abcd model
#' @param ...     parameters to be passed to abcd_qest()
#' @param metric  performance metric, either 'KGE', 'RMSE' or 'NSE'
#' @param na.rm   logical parameter to remove NA values
#' @return        the performance measure of the abcd model
#' @export
abcdCalibrate <- function(..., Q.obs, metric = "KGE", na.rm = FALSE) {

    # Estimated streamflow
    Q.est <- abcd_qest(...)

    # Exclude missing data from calibration
    if (na.rm == TRUE) {
        if (any(is.na(Q.obs))) {
            indNA <- which(is.na(Q.obs))
            Q.obs <- Q.obs[-indNA]
            Q.est <- Q.est[-indNA]
        }
    }

    # ROOT MEAN SQUARE ERROR (RMSE)
    if (metric == "RMSE") {
        Val <- sqrt(mean((Q.obs - Q.est)^2))
    }


    # KLING-GUPTA EFFICIENCY (KGE)
    if (metric == "KGE") {
        cc <- cor(Q.obs, Q.est)
        beta <- mean(Q.est)/mean(Q.obs)
        alpha <- sd(Q.est)/sd(Q.obs)
        Val <- 1 - (1 - sqrt((cc - 1)^2 + (alpha - 1)^2 + (beta - 1)^2))
    }

    # NASH SUTCLIFFE EFFICIENCY (NSE)
    if (metric == "NSE") {
        # Nash-Sutcliffe efficiency
        NS1 <- mapply(function(x, y) (x - y)^2, Q.obs, Q.est)
        NS2 <- mapply(function(x, y) (x - y)^2, Q.obs, mean(Q.obs))
        Val <- sum(NS1)/sum(NS2)
    }

    return(Val)

}