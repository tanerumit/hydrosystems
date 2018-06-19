
#### DISSAGGREGATION OF CLIMATE DATA +++++++++++++++++++++++++++++++++++++++++++

# Monthly ratios for precipitation 
mratios <- hist_climate_avg %>%
  group_by(year, month) %>%
  summarize(prcp = sum(prcp)) %>%
  group_by(year) %>%
  mutate(ratio = prcp/sum(prcp)) %>%
  select(year, month, ratio) %>% spread(month, ratio) 

#Stochastic precipitation
prcp_st <- stoc_traces_prcp %>% as.matrix()
sim_num <- ncol(prcp_st)
sim_len <- nrow(prcp_st)

#List to store climate realizations
hist_climate_rlz <- vector("list", sim_num) 

#Loop over each stochastic realization
pb <- txtProgressBar(min = 1, max = sim_num, style = 3)
for (s in 1:sim_num) {
  
  #Grab the current stochastic realization
  prcp_st_curr <- prcp_st[,s]
  
  # For each year simulated, find the most similar year from the historical record 
  similar_yr_index <- sapply(prcp_st_curr, function(x) 
    which.min(sqrt((x - hist_climate_avg_yr$prcp)^2)))
  
  # Prepare data-frame showing total prcp and the most similar years 
  df <- data_frame(index = 1:sim_len, prcp = prcp_st_curr) %>%
    mutate(similar_yr = hist_climate_avg_yr$year[similar_yr_index]) 
  
  # Bind monthly ratios from the historical record for each similar year 
  df %<>% left_join(mratios, by = c("similar_yr" = "year")) %>%
    gather(key = month, value = ratio, 4:15) %>%
    mutate(month = as.numeric(month)) 

  # Calculate monthly prcp by multiplying sim. annual prcp /w monthly ratios
  df %<>% mutate(prcp_mo = ratio * prcp) %>%
    arrange(index, month) %>%
    dplyr::select(index, month, prcp, prcp_mo) 
  
  # Sample a new month from historical data using KNN algorithm
  df %<>% mutate(KNN_yr = NA) 
  
  # Loop through every row of the file
  for (k in 1:nrow(df)) {
    
    #Filter current months
    hist_prcp_curr  <- filter(hist_climate_avg_mon, month == df$month[k])
    
    #Use KNN algorithm to sample a new monthly precip from the historical data
    KNN_index  <- kNearestNeigboors(x = df$prcp_mo[k], 9, hist_prcp_curr$prcp)


    #Find the year of sampled value of historical monthly precip.
    df$KNN_yr[k] <- hist_prcp_curr$year[KNN_index]
  }
  
  # Calculate scaling factors for precip
  df2 <- df %>% dplyr::select(index, month, KNN_yr, prcp_mo) %>%
    left_join(hist_climate_avg_mon, by = c("KNN_yr" = "year", "month")) %>%
    mutate(prcp_mo_scale = ifelse(prcp == 0, 0, prcp_mo/prcp)) %>%
    select(index, month, KNN_yr, prcp_mo_scale) 
  
  #Obtain multi-variable, multi-site climate realizations (daily/monthly)
  #(index is the serial years from 1:n)
  
  hist_climate_rlz[[s]] <- df2 %>%
    left_join(hist_climate, by = c("KNN_yr" = "year", "month")) %>%  
    mutate(year = index + hist_climate$year[1] - 1) %>%
    mutate(prcp = prcp * prcp_mo_scale) %>%
    dplyr::select(id, year, month, day, prcp, tavg, tmax, tmin)

  setTxtProgressBar(pb, s)
}
close(pb)

rlz_num <- length(hist_climate_rlz)
loc_num <- length(unique(hist_climate_rlz[[1]]$id))









