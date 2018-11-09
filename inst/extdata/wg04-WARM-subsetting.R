
### 3) SUBSET ARIMA-SIMULATED ANNUAL PRECIP ------------------------------------

#Load datasets
#load("./input/warm_hist_prcp.Rdata")
#load("./input/warm_sim_initial.Rdata")

# Descriptive statistics of observed and simulated time-series
stats_obs <- data_frame(mean = mean(ANNUAL_PRCP), sdev = sd(ANNUAL_PRCP))
stats_obs$cor1 <- acf(ANNUAL_PRCP, lag.max = 1, type="correlation", plot=F)[[1]][2]

stats_sim_corr <- PRCP_FINAL_ANNUAL_SIM %>% as_tibble() %>%
  gather(key = variable, value = value) %>%
  group_by(variable) %>%
  nest(.key = "cor1") %>% 
  mutate(cor1 = purrr::map(cor1, ~acf(., lag.max = 1, type="correlation", plot=F)[[1]][2])) %>%
  unnest(cor1)

stats_sim <- PRCP_FINAL_ANNUAL_SIM %>% as_tibble() %>%
  gather(key = variable, value = value) %>%
  group_by(variable) %>%
  summarize(mean = mean(value), sdev = sd(value)) %>%
  left_join(stats_sim_corr, by = "variable")

# Indices of non-negative precip values
sub_nonneg <- which(!sapply(1:sim_num, function(x) any(PRCP_FINAL_ANNUAL_SIM[,x] < 0)))

# Indices of realizations with matching means
rng_mean <- c(stats_obs$mean*(1 - mean_lower), stats_obs$mean*(1 + mean_upper))
sub_mean <- which((stats_sim$mean > rng_mean[1]) & (stats_sim$mean < rng_mean[2]))

# Indices of realizations with matching st. deviation
rng_sdev <- c(stats_obs$sdev*(1 - sdev_lower), stats_obs$sdev*(1 + sdev_upper))
sub_sdev <- which((stats_sim$sdev > rng_sdev[1]) & (stats_sim$sdev < rng_sdev[2]))

# Indices of realizations with matching autocorrelation
rng_cor1 <- c(stats_obs$cor1*(1 - corr_lower), stats_obs$cor1*(1 + corr_upper))
sub_cor1 <- which((stats_sim$cor1 > rng_cor1[1]) & (stats_sim$cor1 < rng_cor1[2]))

# Find series matching all criteria
if (corr_subset == TRUE) {
  sub_clim <- Reduce(intersect, list(sub_mean, sub_sdev, sub_nonneg, sub_cor1))
} else {
  sub_clim <- Reduce(intersect, list(sub_mean, sub_sdev, sub_nonneg))
}

# Obtain the final stochast realizations 
if (length(sub_clim) < nvar_num) {
  print(paste0("Natural variability traces: ", length(sub_clim)))
  set.seed(123); sub_clim_sample <- sample(sub_clim, length(sub_clim)) 
} else {
  print(paste0("Natural variability traces: ", nvar_num))
  set.seed(123); sub_clim_sample <- sample(sub_clim, nvar_num) 
}

stoc_traces_prcp   <- PRCP_FINAL_ANNUAL_SIM[,sub_clim_sample]
stoc_traces_power  <- POWER_SPECTRUM_PRCP_ARIMA_SIM[, sub_clim_sample]
############################ DIAGNOSTIC PLOTS ##################################


#### Comparison of selected traces to observed precipitation

# Wavelet spectra
p <- ggWaveletSpectra(period=period, sig=signif_GWS, obs=GWS, sim=stoc_traces_power) +
  scale_x_continuous(breaks=seq(5,45,10), limits = c(0, 50), expand=c(0,0)) +
  scale_y_log10(labels = comma, breaks = c(1, 10, 20, 50, 100, 250, 500, 1000) * 10^3) +
  theme(panel.grid.minor = element_blank())
ggsave(paste0(plotsDir,"climate-traces/prcp_annual_spectral_sim_subset.png"), height = 6, width = 6)

# Descriptive statistics of simulated series
p1 <- ggplot(mapping = aes(x=mean, y=sdev)) +
  theme_bw(base_size = 11) +
  geom_point(data = stats_sim, color = "black", alpha = 0.2, size = 0.7) +
  geom_point(data = stats_sim[sub_clim_sample,], stroke = 1,
             fill = "green", size = 2, shape = 21, color = "black", alpha = 0.8) +
  geom_point(data = stats_obs, stroke = 1,
             color = "black", size = 2, shape = 21, fill = "blue", alpha = 0.8) +
  labs(y = "std. deviation", x = "mean") 

ggsave(paste0(plotsDir,"climate-traces/prcp_annual_sim_stats1.png"), height = 4, width = 4)

p2 <- p1 %+% aes(x = mean, y = cor1) + labs(y = "correlation (lag1)", x = "mean") 
ggsave(paste0(plotsDir,"climate-traces/prcp_annual_sim_stats2.png"), height = 4, width = 4)


# ------------------------------------------------------------------------------ 
