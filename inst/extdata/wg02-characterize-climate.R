

loc_area  <- grid_area / sum(grid_area)
loc_num   <- hru_num
lat_list  <- sapply(1:hru_num, function(x) formatC(grid_lat[x], format = 'f', flag='0', digits = 3))
lon_list  <- sapply(1:hru_num, function(x) formatC(grid_lon[x], format = 'f', flag='0', digits = 3))
loc_label <- sapply(1:hru_num, function(x) paste0("lat",lat_list[x],"_lon",lon_list[x]))

################################################################################

#### PROCESSING OF HISTORICAL CLIMATE DATA

#Additional date-time tables
hist_date_seq <- seq.Date(hist_date_beg, hist_date_end, by = date_interval)
hist_date_mat <- data_frame(year = year(hist_date_seq), 
  month = month(hist_date_seq), day = day(hist_date_seq))

#Subset climate record for the analysis period
hist_climate_fullperiod <- hru_clim_fullperiod %>% bind_rows(.id = "id")
hist_climate <- hist_climate_fullperiod %>% 
  right_join(hist_date_mat, by = c("year", "month", "day")) 

### Spatially/temporally averaged climate data
hist_climate_mon <- hist_climate %>% 
  group_by(id, year, month) %>%
  summarize_at(vars(climate_vars_all), mean) %>%
  mutate(prcp = prcp * days_in_month(month))

hist_climate_yr <- hist_climate_mon %>% group_by(id, year) %>%
  summarize_at(vars(climate_vars_all), mean) %>% mutate(prcp = prcp * 12)

### Spatially-averaged climate data
tempf <- function(input, area) input * area
hist_climate_avg <- hist_climate %>%
  mutate(area = loc_area[as.numeric(id)]) %>%
  mutate_at(vars(climate_vars_all), funs(tempf(., area))) %>%
  mutate(id = 1) %>%
  group_by(year, month, day) %>%
  summarize_at(vars(climate_vars_all), funs(sum))

hist_climate_avg_mon <- hist_climate_mon %>%
  mutate(area = loc_area[as.numeric(id)]) %>%
  mutate_at(vars(climate_vars_all), funs(tempf(., area))) %>%
  group_by(year, month) %>%
  summarize_at(vars(climate_vars_all), funs(sum))
  
hist_climate_avg_yr <- hist_climate_yr %>% 
  mutate(area = loc_area[as.numeric(id)]) %>%
  mutate_at(vars(climate_vars_all), funs(tempf(., area))) %>%
  group_by(year) %>%
  summarize_at(vars(climate_vars_all), funs(sum))

# Monthly ratios for precipitation 
prcp_monthly_ratios <- hist_climate_avg %>%
  group_by(year, month) %>%
  summarize(prcp = sum(prcp)) %>%
  group_by(year) %>%
  mutate(ratio = prcp/sum(prcp)) %>%
  select(year, month, ratio) %>% spread(month, ratio) 

#Area-averaged annual precipitation and temperature
ANNUAL_PRCP_org <- as.numeric(hist_climate_avg_yr$prcp)
ANNUAL_PRCP <- ANNUAL_PRCP_org 
ANNUAL_TAVG_org <- as.numeric(hist_climate_avg_yr$tavg)
ANNUAL_TAVG <- ANNUAL_TAVG_org

write_csv(path = paste0(inputsDir, basin_name, "_hist_climate_month_avg.csv"), 
  x = hist_climate_avg_mon)

## *****************************************************************************
## ANALYSIS OF HISTORICAL CLIMATE DATA
## *****************************************************************************

### Ggplot dataframes
gg_hist_climate_yr <- hist_climate_yr %>% ungroup() %>%
  mutate(id = factor(id, levels = 1:length(unique(.$id)), 
    labels = loc_label)) 

gg_hist_climate_mon <- hist_climate_mon %>% ungroup() %>%
  mutate(id = factor(id, levels = 1:length(unique(.$id)), 
    labels = loc_label)) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb)) 

if(length(loc_label) > 16) {
  
  sample_num <- sample(length(loc_label), size = 16)
  gg_hist_climate_yr  <- filter(gg_hist_climate_yr, id %in% loc_label[sample_num])
  gg_hist_climate_mon <- filter(gg_hist_climate_mon, id %in% loc_label[sample_num])
  
}

##################### SPATIAL CLIMATE DATA -------------------------------------

########### Annual time-series  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p <- ggplot(gg_hist_climate_yr, aes(x = year)) +
  geom_line() +
  facet_wrap(~ id, scales = "free") +
  geom_smooth(method='lm',formula = y ~ x, fullrange=TRUE) +
  labs(x = "year")

p <- p %+% aes(y = prcp) + labs(y = prcp_unit)
ggsave(filename = paste0(plotsDir,"historical-climate/prcp_annual_trends.png"), 
  height = 8, width = 8)

p <- p %+% aes(y = tavg) + labs(y = tavg_unit)
ggsave(filename =  paste0(plotsDir,"historical-climate/tavg_annual_trends.png"), 
  height = 8, width = 8)

########### mann-Kendall Analaysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

gg_hist_climate_yr %>%
  group_by(id) %>%
  nest() %>%
  mutate(MK_tau = map(data, ~ Kendall::MannKendall(.$prcp)[[1]])) %>%
  mutate(p_value = map(data, ~ Kendall::MannKendall(.$prcp)[[2]])) %>%
  unnest(MK_tau, p_value) %>% select(-data) %>%
  mutate(MK_tau = round(MK_tau, 3), p_value = round(p_value, 5)) %>%
  tableGrob(theme = ttheme_minimal(base_size = 11)) %>%
  ggsave(filename = paste0(plotsDir,"historical-climate/prcp_annual_mkendall.png"))

gg_hist_climate_yr %>%
  group_by(id) %>%
  nest() %>%
  mutate(MK_tau = map(data, ~ Kendall::MannKendall(.$tavg)[[1]])) %>%
  mutate(p_value = map(data, ~ Kendall::MannKendall(.$tavg)[[2]])) %>%
  unnest(MK_tau, p_value) %>% select(-data) %>%
  mutate(MK_tau = round(MK_tau, 3), p_value = round(p_value, 10)) %>%
  tableGrob(theme = ttheme_minimal(base_size = 11)) %>%
  ggsave(filename = paste0(plotsDir,"historical-climate/tavg_annual_mkendall.png"))


########### #Correlation matrix ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Precipitation
(gg_hist_climate_mon %>%
  select(id, prcp, year, month) %>% spread(key = id, value = prcp) %>%
  select(-year, -month) %>%
  cor(., use="pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, outline.col="white", type="lower",
    lab = TRUE, lab_size = 3, colors = c("tomato2", "white", "springgreen3"), 
    title= "Monthly precipitation - correlation matrix", 
    ggtheme = theme_minimal)) %>%
  ggsave(filename = paste0(plotsDir,"historical-climate/prcp_monthly_corrmat.png"), 
    height = 8, width = 12)

# Temperature
(gg_hist_climate_mon %>%
  select(id, tavg, year, month) %>% spread(key = id, value = tavg) %>%
  select(-year, -month) %>%
  cor(., use="pairwise.complete.obs") %>%
  ggcorrplot::ggcorrplot(hc.order = TRUE, outline.col="white", type="lower",
    lab = TRUE, lab_size = 3, colors = c("tomato2", "white", "springgreen3"), 
     title= "Monthly mean temperature - correlation matrix", 
    ggtheme = theme_minimal)) %>%
  ggsave(filename = paste0(plotsDir,"historical-climate/tavg_monthly_corrmat.png"), 
    height = 8, width = 12)


#-------------------------------------------------------------------------------


##################### SPATIALLY-AVERAGED CLIMATE DATA --------------------------

#Spatially-avareged annual precip

annual_prcp_yrs  <- pretty(range(year(hist_date_seq)), n = 5)
annual_prcp_bins <- pretty(range(unique(year(hist_date_seq)), n = 5))
df <- data_frame(x = unique(year(hist_date_seq)), y = ANNUAL_PRCP)

tsData <- ts(ANNUAL_PRCP, frequency = 1, 
  start = c(year(hist_date_beg), month(hist_date_beg)))

### Time-series
p1 <- ggplot(df, aes(x, y)) + geom_line() + labs(x = "year", y = prcp_unit) + 
  scale_x_continuous(breaks = annual_prcp_yrs) 
ggsave(filename = paste0(plotsDir,"historical-climate/prcp_annual_trends_avg.png"), 
  height = 4, width = 8)

### Time-series (with trend line)  
(p1 + geom_smooth(method = "lm")) %>%
ggsave(filename = paste0(plotsDir,"historical-climate/prcp_annual_trends_avg.png"), 
  height = 4, width = 8)

# Check Normality ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# Boxplot
p2 <- ggplot(df, aes(x = y)) + stat_bin(color="blue", fill="gray90", bins = 12) + 
  labs(x = prcp_unit) 

# QQ plot
p3 <- ggplot(df, aes(sample = y)) +
    #stat_qq_band(distribution = "norm") +
    stat_qq_line(distribution = "norm") +
    stat_qq_point(distribution = "norm") +
    labs(x = "Theoretical Quantiles", y = "Sample Quantiles")

p <- cowplot::plot_grid(p2, p3, nrow = 1, labels= c("b)", "c)"), align = "v")
p <- cowplot::plot_grid(p1, p, nrow = 2, labels = c("a)",""))
ggsave(paste0(plotsDir,"historical-climate/prcp_annual_avg_normality.png"), height = 8, width = 8)

#### Augmented Dickey-Fuller Test (p-value < 0.05 indicates stationary)
#tseries::adf.test(tsData) 

#### Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test 
# ho: x is level or trend stationary
#tseries::kpss.test(tsData)


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
(ggWaveletSpectra(period = period, sig = signif_GWS, obs = GWS) +
  scale_x_continuous(breaks=seq(0,40,10), expand=c(0,0)) +
  scale_y_log10(labels = comma, 
                breaks = c(1, 10, 20, 50, 100, 250, 500, 1000) * 10^3)) %>%
ggsave(filename = paste0(plotsDir,"historical-climate/prcp_annual_avg_spectral.png"), 
  height = 4, width = 8)

####### SEASONALITY/INTRA-ANNUAL VARIABILITY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Monthly boxplots of temp and precip
p <- ggplot(gg_hist_climate_mon, aes(x = month, y = prcp, group = month)) + 
  geom_boxplot(color = "steelblue") +
  labs(x = NULL, y = "Precip. (mm)"); p
ggsave(paste0(plotsDir,"historical-climate/prcp_monthly_avg_boxplot.png"), height = 4, width = 8)

p <- ggplot(gg_hist_climate_mon, aes(x = month, y = tavg, group = month)) + 
  geom_boxplot(color = "steelblue") +
  labs(x = NULL, y = "Tavg (Deg C)"); p
ggsave(paste0(plotsDir,"historical-climate/tavg_boxplot_avg_monthly.png"), height = 4, width = 8)


#Monthly box-plots of temp and precip
df <- hist_climate_avg_mon %>%  ungroup() %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb)) %>%
  mutate(year = as.integer(year))
df$yearw <- cut(df$year, breaks = seq(1940, 2010, 20), dig.lab = 5)

p <- ggplot(df, aes(y = month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.03) +
  scale_fill_viridis(alpha = 1, begin = 0, end = 1, direction = -1,
  discrete = FALSE, option = "A") +
  labs(y = "Month")

# Joyplots of temp and precip (experimental)

p %+% aes(x= prcp) + labs(x = prcp_unit)
ggsave(paste0(plotsDir,"historical-climate/prcp_monthly_ridges.png"), height = 4, width = 8)

p %+% aes(x= prcp) + labs(x = tavg_unit)
ggsave(paste0(plotsDir,"historical-climate/tavg_monthly_ridges.png"), height = 4, width = 8)

p %+% aes(x= prcp, y = yearw) + labs(x = prcp_unit,  y = "time-window")
ggsave(paste0(plotsDir,"historical-climate/prcp_monthly_avg_ridges.png"), height = 4, width = 8)

p %+% aes(x= tavg, y = yearw) + labs(x = tavg_unit,  y = "time-window")
ggsave(paste0(plotsDir,"historical-climate/tavg_monthly_avg_ridges.png"), height = 4, width = 8)

#-------------------------------------------------------------------------------


