
## *****************************************************************************
## ANALYSIS OF HISTORICAL CLIMATE DATA
## *****************************************************************************

### GGplot parameters
prcp_unit <- "precip (mm)"
tavg_unit <- expression(paste("Temperature (",degree,"C)"))

### Ggplot dataframes
gg_hist_climate_yr <- hist_climate_yr %>% ungroup() %>%
  mutate(id = factor(id, levels = 1:length(unique(.$id)), 
    labels = loc_label)) 

gg_hist_climate_mon <- hist_climate_mon %>% ungroup() %>%
  mutate(id = factor(id, levels = 1:length(unique(.$id)), 
    labels = loc_label)) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb)) 

if(length(loc_label) > 16) {
  
  gg_hist_climate_yr  <- filter(gg_hist_climate_yr, id %in% loc_label[1:16])
  gg_hist_climate_mon <- filter(gg_hist_climate_mon, id %in% loc_label[1:16])
  
}


################################################################################
###################### SPATIALLY-AVERAGED CLIMATE DATA #########################
################################################################################


# Year intervals and annual climate variables
axis_annual_interval  <- pretty(range(year(hist_date_seq)), n = 5)
start_ts <- c(year(hist_date_beg), month(hist_date_beg))
prcp_annual_ts <- ts(ANNUAL_PRCP, frequency = 1, start_ts)
tavg_annual_ts <- ts(ANNUAL_TAVG, frequency = 1, start_ts)


# TREND ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Time-series
p0 <- ggplot(hist_climate_avg_yr, aes(x = year, y = prcp)) + 
  geom_line() + labs(x = "year", y = prcp_unit) + 
  scale_x_continuous(breaks = axis_annual_interval) 
p1 <- p0 + geom_smooth(method = "lm") 
ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_avg_series.png"), 
  plot = p0, height = 4, width = 8)
ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_avg_series_trend.png"), 
  plot = p1, height = 4, width = 8)

#Man-kendall analysis
prcp_hist_avg_mk <- Kendall::MannKendall(ANNUAL_PRCP)

# Histogram
p2 <- ggplot(hist_climate_avg_yr, aes(x = prcp)) + 
  stat_bin(color="blue", fill="gray90", bins = 12) + labs(x = prcp_unit) 

# QQ plot
p3 <- ggplot(hist_climate_avg_yr, aes(sample = prcp)) +
    #stat_qq_band(distribution = "norm") +
    stat_qq_line(distribution = "norm") +
    stat_qq_point(distribution = "norm") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

p <- cowplot::plot_grid(p2, p3, nrow = 1, labels= c("b)", "c)"), align = "v")
p <- cowplot::plot_grid(p1, p, nrow = 2, labels = c("a)",""))
ggsave(paste0(wegen_plots_dir,"/prcp_hist_avg_normality.png"), height = 8, width = 8)
p_prcp_hist_annual_normality <- p

#### Augmented Dickey-Fuller Test (p-value < 0.05 indicates stationary)
#tseries::adf.test(prcp_annual_ts) 

#### Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
# ho: x is level or trend stationary
#tseries::kpss.test(prcp_annual_ts)

# Temperature 
p0 <- ggplot(hist_climate_avg_yr, aes(x = year, y = tavg)) + 
  geom_line() + labs(x = "year", y = tavg_unit) + 
  scale_x_continuous(breaks = axis_annual_interval) 
p1 <- p0 + geom_smooth(method = "lm") 
ggsave(filename = paste0(wegen_plots_dir,"/tavg_hist_avg_series.png"), 
  plot = p0, height = 4, width = 8)
ggsave(filename = paste0(wegen_plots_dir,"/tavg_hist_avg_series_trend.png"), 
  plot = p1, height = 4, width = 8)

# Man-kendall statistics
tavg_hist_avg_mk <- Kendall::MannKendall(ANNUAL_TAVG)

# SPECTRAL ANALYSIS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CLIMATE_VARIABLE <-  ANNUAL_PRCP
CURRENT_CLIMATE_VARIABLE_org <- CLIMATE_VARIABLE

output <- waveletAnalysis(variable = CLIMATE_VARIABLE, siglvl = 0.95)
list2env(output,globalenv())

#Power spectra of observed annual precip
POWER_SPECTRUM_PRCP_OBS <- GWS
#Significance level for observed annual precip
POWER_SPECTRUM_PRCP_SIGNIFICANCE <- signif_GWS
#Power spectra periods
POWER_SPECTRUM_PRCP_PERIOD <- period

#Plot wavelet spectrum
p <- ggWaveletSpectra(period = period, sig = signif_GWS, obs = GWS) +
  scale_x_continuous(breaks=seq(0,40,10), expand=c(0,0)) +
  scale_y_log10(labels = comma, 
                breaks = c(1, 10, 20, 50, 100, 250, 500, 1000) * 10^3)

p %>% ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_avg_spectral.png"), 
  height = 4, width = 8)

p_prcp_hist_spectra <- p

####### SEASONALITY/INTRA-ANNUAL VARIABILITY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Monthly boxplots of precip
p <- ggplot(gg_hist_climate_mon, aes(x = month, y = prcp, group = month)) + 
  geom_boxplot(color = "steelblue") +
  labs(x = NULL, y = "Precip. (mm)")

p_prcp_monthly_boxplot <- p 
ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_avg_seasonal_boxplot.png"), 
  plot = p, height = 4, width = 8)

# Monthly boxplots of tavg
p <- ggplot(gg_hist_climate_mon, aes(x = month, y = tavg, group = month)) + 
  geom_boxplot(color = "steelblue") +
  labs(x = NULL, y = "Tavg (Deg C)")

p_tavg_monthly_boxplot <- p 
ggsave(paste0(wegen_plots_dir,"/tavg_hist_avg_seasonal_boxplot.png"), 
  plot = p, height = 4, width = 8)

# Joyplots of temp and precip (experimental)
df <- hist_climate_avg_mon %>%  ungroup() %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb)) %>%
  mutate(year = as.integer(year))
df$yearw <- cut(df$year, breaks = seq(1940, 2010, 20), dig.lab = 5)

p <- ggplot(df, aes(y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.03) +
  scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1,
  discrete = FALSE, option = "A") +
  labs(y = "Month")

p %+% aes(x= prcp) + labs(x = prcp_unit)
ggsave(paste0(wegen_plots_dir,"/prcp_hist_avg_seasonal_ridges.png"), height = 4, width = 8)

p %+% aes(x= tavg) + labs(x = tavg_unit)
ggsave(paste0(wegen_plots_dir,"/tavg_hist_avg_seasonal_ridges.png"), height = 4, width = 8)


################################################################################
###################### SPATIAL CLIMATE DATA ####################################
################################################################################


########### Annual time-series  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

p <- ggplot(gg_hist_climate_yr, aes(x = year)) +
  geom_line() +
  facet_wrap(~ id) +
  geom_smooth(method='lm',formula = y ~ x, fullrange=TRUE) +
  labs(x = "year")

# Precipitation
p <- p %+% aes(y = prcp) + labs(y = prcp_unit)
ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_trends.png"), 
  height = 8, width = 8)
p_prcp_hist_annual_ts <- p 

# Temperature
p <- p %+% aes(y = tavg) + labs(y = tavg_unit)
ggsave(filename =  paste0(wegen_plots_dir,"/tavg_hist_trends.png"), 
  height = 8, width = 8)
p_tavg_hist_annual_ts <- p 

# Mann-Kendall: Annual precipitation
p <- gg_hist_climate_yr %>%
  group_by(id) %>%
  nest() %>%
  mutate(MK_tau = map(data, ~ Kendall::MannKendall(.$prcp)[[1]])) %>%
  mutate(p_value = map(data, ~ Kendall::MannKendall(.$prcp)[[2]])) %>%
  unnest(MK_tau, p_value) %>% select(-data) %>%
  mutate(MK_tau = round(MK_tau, 3), p_value = round(p_value, 5)) 

p %>%
  tableGrob(theme = ttheme_minimal(base_size = 11)) %>% 
  ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_mkendall.png"))
p_prcp_hist_annual_mk <- p

# Mann-Kendall: Annual temperature
p <- gg_hist_climate_yr %>%
  group_by(id) %>%
  nest() %>%
  mutate(MK_tau = map(data, ~ Kendall::MannKendall(.$tavg)[[1]])) %>%
  mutate(p_value = map(data, ~ Kendall::MannKendall(.$tavg)[[2]])) %>%
  unnest(MK_tau, p_value) %>% select(-data) %>%
  mutate(MK_tau = round(MK_tau, 3), p_value = round(p_value, 10))

p %>%
  tableGrob(theme = ttheme_minimal(base_size = 11)) %>%
  ggsave(filename = paste0(wegen_plots_dir,"/tavg_hist_mkendall.png"))
p_tavg_hist_annual_mk <- p

########### #Correlation matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Precipitation
p <- gg_hist_climate_mon %>%
  select(id, prcp, year, month) %>% spread(key = id, value = prcp) %>%
  select(-year, -month) %>%
  cor(., use="pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, outline.col="white", type="lower",
    lab = TRUE, lab_size = 4, colors = c("#6D9EC1", "white", "#E46726"), 
    title= "Monthly precipitation - correlation matrix", 
    ggtheme = theme_minimal)

p %>% ggsave(filename = paste0(wegen_plots_dir,"/prcp_hist_corrmat.png"), 
    height = 8, width = 12)
p_prcp_hist_mon_corr <- p

# Temperature
p <- gg_hist_climate_mon %>%
  select(id, tavg, year, month) %>% spread(key = id, value = tavg) %>%
  select(-year, -month) %>%
  cor(., use="pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, outline.col="white", type="lower",
    lab = TRUE, lab_size = 4, colors = c("#6D9EC1", "white", "#E46726"), 
     title= "Monthly mean temperature - correlation matrix", 
    ggtheme = theme_minimal)

p %>% ggsave(filename = paste0(wegen_plots_dir,"/tavg_hist_corrmat.png"), 
    height = 8, width = 12)
p_tavg_hist_mon_corr <- p


