
#-------------------------------------------------------------------------------$

#' @title "Weighted Anomaly Standardized Precipitation" WASP index
#' @description WASP is an acronym for the "Weighted Anomaly Standardized 
#' Precipitation" index and is based solely on monthly precipitation data. 
#' It gives an estimate of the relative deficit or surplus of precipitation for 
#' different time intervals ranging from 1 to 12 months.
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param N PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @export 
waspIndex <- function(data = NULL, N = NULL) {
  
  require(zoo); require(dplyr)
  
  # Mean and standard deviation of precip in each month
  month_stats  <- data %>% 
    group_by(month) %>% 
    summarize(mean_mo = mean(prcp), sd_mo = sd(prcp))
  
  annual_stats <- data %>% 
    group_by(year) %>% 
    summarize(mean_yr = mean(prcp))
  
  # Calculate monthly index values for each grid cell
  out <- data %>%
    left_join(month_stats, by = "month") %>%
    left_join(annual_stats, by = "year") %>%
    mutate(m_ratio = ifelse(mean_yr == 0, 0, (mean_mo/mean_yr))) %>%
    mutate(sn   = (prcp - mean_mo)/sd_mo*(mean_mo/mean_yr)) %>%
    mutate(val  = rollsum(sn, k = N, align = "right", fill = NA)) %>%
    mutate(wasp = val / sd(val, na.rm = TRUE)) %>%
    select(year, month, prcp, wasp)

  return(out)
}