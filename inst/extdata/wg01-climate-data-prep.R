
## *****************************************************************************
## CLIMATE DATA PREPARATION 
## *****************************************************************************


# STATION/GROUND DATA ----------------------------------------------------------

### Specify hru info file and climate data directory
climate_data_info <- readr::read_csv("./input/weap_climate_data_info.csv")
climate_data_dir <- "./input/weap_climate0/"
climate_vars_input <- c("prcp", "tavg", "tmin", "tmax")

# Climate file names  
loc_file  <- climate_data_info$filename 
loc_label <- stringr::str_sub(loc_file, 1, stringr::str_length(loc_file)-4)
loc_id    <- 1:length(unique(loc_file))
loc_area  <- climate_data_info$area / sum(climate_data_info$area)

days_in_month(1)
# Read in climate data 
hist_climate_fullperiod <- list()
for (i in 1:length(loc_id)) {
  
  hist_climate_fullperiod[[i]] <- read_csv(paste0(climate_data_dir,loc_file[i]), 
    skip = 3, col_names = FALSE) 
  colnames(hist_climate_fullperiod[[i]]) <- c("year", "month", climate_vars_input)

  hist_climate_fullperiod[[i]] %<>% mutate(day = 1) %>% 
    mutate(prcp = prcp / days_in_month(month)) %>%
    select(year, month, day, one_of(climate_vars_input))
}
hist_climate_fullperiod %<>% bind_rows(.id = "id")

### Save output to file
save(hist_climate_fullperiod, loc_file, loc_area, loc_label, climate_vars_input, 
  climate_data_info, climate_data_dir,
  file = "./input/hist_climate_data_ground.Rdata")

################################################################################
################################################################################

#-------------------------------------------------------------------------------
# GRIDDED CLIMATE DATA ---------------------------------------------------------

### Specify hru info file and climate data directory
climate_data_info <- read.table("./input/HRUinfo_025deg_Katima.txt")
climate_data_dir <- "E:/Data/Area/Batoka/ClimateForcing/CHIRPS_Adj/forHYMOD/CHIRPS_Adj_"
climate_vars_input <- c("prcp", "tavg")
  
### Read-in gridded climate data
loc_lat   <- climate_data_info[, 1] # latitude information
loc_lon   <- climate_data_info[, 2] # longigute information
loc_area  <- climate_data_info[, 3] / 100 # area information
loc_label <- paste(loc_lat, loc_lon, sep = "_")
loc_id    <- 1:length(loc_lat)

### Read-in climate data for all HRUs
hist_climate_fullperiod <- list()
for (n in 1:length(loc_id)) {

  lat  <- formatC(loc_lat[n], format = 'f', flag='0', digits = 3)
  lon  <- formatC(loc_lon[n], format = 'f', flag='0', digits = 3)

  df <- read.table(paste0(climate_data_dir,lat, "_", lon)) %>% as.matrix()
  colnames(df) <- c("year", "month", "day", "prcp", "tavg")
  hist_climate_fullperiod[[n]] <- df %>% as_tibble() %>%
    select(year, month, day, one_of(climate_vars_input))

}

hist_climate_fullperiod %<>% bind_rows(.id = "id")

### Save output to file
save(hist_climate_fullperiod, loc_file, loc_area, loc_label, climate_vars_input, 
  climate_data_info, climate_data_dir,
  file = "./input/hist_climate_data_gridded.Rdata")




