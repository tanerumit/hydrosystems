
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
###  WEATHER GENERATOR -- MASTER SCRIPT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Cran packages
library(tidyverse); library(magrittr); library(dplyr); library(tidyr);
library(grid); library(gridExtra); library(lubridate); library(Kendall); 
library(qqplotr); library(ggridges); library(viridis)

### Gitlab packages
library(hydrosystems); library(ggHydro)

### KEY SETTINGS #############################################

### weather generator scripts directory
wegen_dir <- "C:/Users/taner/Dropbox/research/scripts/R/wegen"

### Historical climate period
date_interval <- "month"
hist_date_beg <- as.Date("1948/01/1")
hist_date_end <- as.Date("2008/12/31")
hist_date_seq <- seq.Date(hist_date_beg, hist_date_end, by = date_interval)
hist_date_mat <- data_frame(year = year(hist_date_seq), 
  month = month(hist_date_seq), day = day(hist_date_seq))

### Read-in tidy-climate data (ground observations)
load("./input/hist_climate_data_ground.Rdata")

##############################################################

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

## GGplot settings
theme_set(theme_light())

#-------------------------------------------------------------------------------
##################  STEP 1) Characterize historical climate (optional)

### plots directory
plot_dir  <- "./graphics/climate-ground"
source(paste0(wegen_dir, "/wg1-characterize-climate.R"))


#-------------------------------------------------------------------------------
##################  STEP 2) GENERATE ANNUAL PRECIP USING WAVELET AR MODEL

### Key parameters
sim_length  <- 60   # length of each new time-series 
sim_num     <- 20  # number of new climate time-series

## Wavelet model parameters
component_num         <-  1 #number of orthogonal series representing low-freq signal
NUM_PERIODS_COMP1     <-  5 #length of the first component: initial guess
NUM_PERIODS_ALL_COMPS <- c(NUM_PERIODS_COMP1) #length of all components
ALL_SIG_PERIODS       <- c(1,2,3,4,5) 

source(paste0(wegen_dir, "/wg2-WARM-simulation.R"))

#save(PRCP_FINAL_ANNUAL_SIM, POWER_SPECTRUM_PRCP_ARIMA_SIM, sim_length, sim_num,
#     file = "./input/warm_sim_initial.Rdata")

#-------------------------------------------------------------------------------
##################  STEP 3) SUBSET REALIAZATIONS FROM THE INITIAL WARM ARRAY

load("./input/warm_sim_initial.Rdata")

#Set sub-setting criteria
corr_bound <- 0.20 # max varaiability in correlation coefficient
mean_bound <- 0.02 # max variability in mean value
sdev_bound <- 0.02 # max variability in standard deviation

source(paste0(wegen_dir, "/wg3-WARM-subsetting.R"))

#-------------------------------------------------------------------------------
##################  STEP 4) SPATIAL-TEMPORAL DISSAGGREGATION

source(paste0(wegen_dir, "/wg4-WARM-disaggregate.R"))

#Save to file
#write_rds(hist_climate_rlz, path = "./input/hist_climate_rlz.rds", "gz")

#Save final climate series selection 
save(stoc_traces_prcp, stoc_traces_power,  sub_clim_sample,
     file = "./input/warm_results_final.Rdata")

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

source(paste0(wegen_dir, "/wg5-climate-trends.R"))

#Write to file
write_rds(future_clim_rlz, "./input/future_clim_rlz.rds")
