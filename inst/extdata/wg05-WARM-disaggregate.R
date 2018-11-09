
#### DISSAGGREGATION OF CLIMATE DATA +++++++++++++++++++++++++++++++++++++++++++

#Stochastic precipitation
prcp_st <- stoc_traces_prcp %>% as.matrix()
rlz_num <- ncol(prcp_st)
sim_len <- nrow(prcp_st)

#List to store climate realizations
hist_climate_rlz <- vector("list", rlz_num) 

#Loop over each stochastic realization
pb <- txtProgressBar(min = 1, max = rlz_num, style = 3)

for (s in 1:rlz_num) {
  
  #Grab the current stochastic realization
  prcp_st_curr <- prcp_st[,s]
  
  # For each year simulated, find the most similar year from the historical record 
  similar_yr_index <- sapply(prcp_st_curr, function(x) 
    which.min(sqrt((x - hist_climate_avg_yr$prcp)^2)))
  
  # Prepare data-frame showing total prcp and the most similar years 
  df <- data_frame(index = 1:sim_len, prcp = prcp_st_curr) %>%
    mutate(similar_yr = hist_climate_avg_yr$year[similar_yr_index]) 
  
  # Bind monthly ratios from the historical record for each similar year 
  df %<>% left_join(prcp_monthly_ratios, by = c("similar_yr" = "year")) %>%
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
    dplyr::select(id, year, month, day, climate_vars_all) #%>%
    #adjustment for monthly data
    #mutate(prcp = prcp * days_in_month(month))

  setTxtProgressBar(pb, s)
}
close(pb)

