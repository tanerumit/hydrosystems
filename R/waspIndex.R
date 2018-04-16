
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
  
  # Required monthly and annual stats for prcpipitation
  mean_mo  <- data %>% group_by(month) %>% summarize(v = mean(prcp)) %>% pull(v)
  sd_mo    <- data %>% group_by(month) %>% summarize(v = sd(prcp)) %>% pull(v) 
  mean_ann <- data %>% group_by(year) %>% summarize(v = mean(prcp)) %>% pull(v)
  
  # Calculate monthly index values for each gridcell
  out <- data %>% mutate(
    Sn = (prcp - mean_mo[month])/sd_mo[month]*(mean_mo[month]/mean_ann[month])) %>%
    mutate(val = zoo::rollsum(Sn, k = N, align = right, fill = NA)) %>%
    mutate(wasp = val / sd(val, na.rm = TRUE)) %>%
    select(year, month, prcp, wasp)
  
  return(out)
}