

# ------------------------------------------------------------------------------
# ADDITIVE/MULTIPLICATIVE TRENDS -----------------------------------------------

delta_tavg <- seq(delta_tavg_range[1], delta_tavg_range[2], by = delta_tavg_step)
delta_prcp <- seq(delta_prcp_range[1], delta_prcp_range[2], by = delta_prcp_step)

#Total number of mean changes explored
delta_num  <- length(delta_prcp) * length(delta_tavg)

### Prepare master data_frame to store scenario information 
clim_mat <- expandGrid(tavg = delta_tavg, prcp = delta_prcp, rlz = 1:rlz_num) %>%
  arrange(rlz, tavg, prcp) %>%
  mutate(id = 1:n()) %>% 
  dplyr::select(id, rlz, tavg, prcp)

clim_num <- nrow(clim_mat)
sim_yrs_num <- sim_length
future_climate_rlz <- rep(list(vector(mode="list", length=loc_num)), clim_num) 

# Loop through each climate scenario 
pb <- txtProgressBar(min = 1, max = clim_num, style = 3)
for (k in 1:clim_num) {
  
  #Current climate data
  future_climate_i <- hist_climate_rlz[[clim_mat$rlz[k]]]

  #Annual sequence of changes
  delta_tavg_seq_yr <- seq(0, clim_mat$tavg[k], length = sim_yrs_num)
  delta_prcp_seq_yr <- seq(1, clim_mat$prcp[k] , length = sim_yrs_num)
  
  #Apply changes to climate data
  future_climate_rlz[[k]] <- future_climate_i %>% 
    mutate(year = year + future_year_shift) %>%
    mutate(year_ind = year - min(year) + 1) %>% 
    mutate(prcp = prcp * delta_prcp_seq_yr[year_ind], 
           tavg = tavg + delta_tavg_seq_yr[year_ind],
           tmax = tmax + delta_tavg_seq_yr[year_ind],
           tmin = tmin + delta_tavg_seq_yr[year_ind]) %>%
    dplyr::select(-year_ind) %>%
    mutate(tavg = round(tavg, 1), prcp = round(prcp, 1), 
           tmax = round(tmax, 1), tmin = round(tmin, 1)) %>%
    arrange(id, year, month, day)
  
  setTxtProgressBar(pb, k)
}
close(pb)