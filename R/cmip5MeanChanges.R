

CMIP5meanChanges <- function(data, hist.period, proj.period,
    summary.stat = "mean", months.of.year = 1:12) {
  
  require(dplyr)
  
  # Summarize data for each projection
  df <- lapply(names(data), function(x) bind_rows(data[[x]], .id = "model")) %>%
    setNames(names(data)) %>% bind_rows(.id = "scenario") %>% ungroup() 
  
  # Stats for the Historical (hindcast) runs
  data_hist <- df %>% filter(scenario == "historical") %>%
    filter(year %in% hist.period) %>% 
    filter(mon  %in% months.of.year) %>%
    group_by(model) %>% 
    summarize_at(vars(climate.vars), summary.stat) 
  
  # Stats for the future runs (projections)
  data_proj <- df %>% filter(scenario != "historical") %>%
    filter(year %in% proj.period) %>% 
    filter(mon  %in% months.of.year) %>%
    group_by(scenario, model) %>%
    summarize_at(vars(prcp, tavg), summary.stat)
  
  # Delta changes 
  delta_prcp <- data_proj %>%
    select(scenario, model, prcp) %>%
    left_join(select(data_hist, model, hist_prcp = prcp), by = "model") %>%
    mutate(prcp = (prcp - hist_prcp) / hist_prcp * 100)
  
  delta_tavg <- data_proj %>%
    select(scenario, model, tavg) %>%
    left_join(select(data_hist, model, hist_tavg = tavg), by = "model") %>%
    mutate(tavg = tavg - hist_tavg)
  
  delta_clim <- delta_prcp %>%
    left_join(delta_tavg, by = c("scenario", "model")) %>%
    na.omit() %>% select(scenario, model, prcp, tavg)
}