
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  WEATHER GENERATOR -- MASTER SCRIPT
#  Date: June 18, 2018
#  By: Umit Taner
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cran packages
library(tidyverse); library(magrittr); library(dplyr); 
library(tidyr); library(grid); library(gridExtra); 
library(lubridate); library(Kendall); library(qqplotr); 
library(ggridges); library(viridis)

### Gitlab packages
library(hydrosystems); library(ggHydro)

## GGplot settings
theme_set(theme_light())

# Key settings +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

### weather generator scripts and output plots directories
wegen_plots_dir  <- "./graphics/climate-princeton"

### Historical climate data
date_interval <- "month"
hist_date_beg <- as.Date("1948/01/1")
hist_date_end <- as.Date("2008/12/31")
load("./input/hist_climate_data_ground.Rdata")

#date_interval <- "day"
#hist_date_beg <- as.Date("2001/01/1")
#hist_date_end <- as.Date("2015/12/31")
#load("./input/hist_climate_data_gridded.Rdata")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Number of stations/grid cells
loc_num <- length(unique(hist_climate_fullperiod$id))

#Additional date-time tables
hist_date_seq <- seq.Date(hist_date_beg, hist_date_end, by = date_interval)
hist_date_mat <- data_frame(year = year(hist_date_seq), 
  month = month(hist_date_seq), day = day(hist_date_seq))

#Subset climate record for the analysis period
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

# Area-averaged precipitation
ANNUAL_PRCP_org <- as.numeric(hist_climate_avg_yr$prcp)
ANNUAL_PRCP <- ANNUAL_PRCP_org 

#-------------------------------------------------------------------------------
##################  STEP 1) Characterize historical climate 

# Annual trends
# Seasonal plot
# Trend analysis (ManKendall)
# Spectral analysis

#box-cox transformation 
#lambda <- as.numeric(car::powerTransform(ANNUAL_PRCP)$lambda)
#TRANSFORM_DATA <- car::bcPower(ANNUAL_PRCP,lambda)
#ANNUAL_PRCP <- TRANSFORM_DATA

#Characterize climate files
source(system.file("extdata", "wg02-characterize-climate.R", package="hydrosystems"))

#### save warm outputs to file 
#save(POWER_SPECTRUM_PRCP_OBS, period, signif_GWS,
#  file = "./input/warm_hist_prcp.Rdata")

#-------------------------------------------------------------------------------
##################  STEP 2) GENERATE ANNUAL PRECIP USING WAVELET AR MODEL

# Use wavelet AR model to generate new annual precipitation series. 

### Key parameters
sim_length  <- 60    # length of each new time-series 
sim_num     <- 5000  # number of new climate time-series

### Wavelet model parameters
component_num         <-  1 #number of orthogonal series representing low-freq signal
NUM_PERIODS_COMP1     <-  5 #length of the first component: initial guess
NUM_PERIODS_ALL_COMPS <- c(NUM_PERIODS_COMP1) #length of all components
ALL_SIG_PERIODS       <- c(1,2,3,4,5) 

source(system.file("extdata", "wg03-WARM-simulation.R", package="hydrosystems"))

### Save results to file
#save(PRCP_FINAL_ANNUAL_SIM, POWER_SPECTRUM_PRCP_ARIMA_SIM, sim_length, sim_num,
#     file = "./input/wegen/warm_sim_initial.Rdata")

#-------------------------------------------------------------------------------
##################  STEP 3) SUBSET FROM INITIAL WARM REALIZATIONS

#load("./input/warm_sim_initial.Rdata")

### Number of natural variability traces selected
nvar_num <- 5

### Set sub-setting criteria
corr_bound <- 0.200 # max variability in correlation coefficient
mean_bound <- 0.015 # max variability in mean value
sdev_bound <- 0.015 # max variability in standard deviation

source(system.file("extdata", "wg04-WARM-subsetting.R", package="hydrosystems"))

### save to file
save(stoc_traces_prcp, stoc_traces_power,  sub_clim_sample,
     file = "./input/wegen/warm_sim_subsetting.Rdata")

#-------------------------------------------------------------------------------
##################  STEP 4) SPATIAL-TEMPORAL DISSAGGREGATION

### Load from file
#load("./input/warm_sim_subsetting.Rdata")

#source(system.file("extdata", "wg05-WARM-disaggregate.R", package="hydrosystems"))

#(system.file("extdata", "wg05-WARM-disaggregate.R", package="hydrosystems"))
source("C:/Users/Umit/Dropbox/research/scripts/R/hydrosystems/inst/extdata/wg05-WARM-disaggregate.R")

#Save to file
#write_rds(hist_climate_rlz, path = "./input/wegen/hist_climate_rlz.rds", "gz")


#-------------------------------------------------------------------------------
##################  STEP 5) CLIMATE CHANGE TRENDS

# Baseline, natural variability realizations
#hist_climate_rlz <- read_rds(path = "./input/hist_climate_rlz.rds")

future_year_shift <- 70

#Mean temperature change: range and step
delta_tavg_range <- c(0,8)
delta_tavg_step  <- 1

#Mean precipitation change: range and step
delta_prcp_range <- c(0.6,1.4)
delta_prcp_step  <- 0.1

source(system.file("extdata", "/wg06-climate-trends.R", package="hydrosystems"))

#Write to file
write_rds(future_climate_rlz, "./input/future_climate_rlz.rds")


