
## This script is tied to the outputs of the CMIP5 TOOL (Wi, 2017)

gcmDataTransform <- function(
  path = NULL, 
  gridInfo = "cmip5grid.xlsx", 
  climateVars = c("prcp", "tavg", "tmax", "tmin"),
  scenarios = c("historical", "rcp26", "rcp45", "rcp60", "rcp85")) {
  
  #browser()
  
  require(readxl)
  require(dplyr)
  
  file_header <- c("coord", "year", "mon", climateVars)

  #List to store the results
  out <- list()
  
  #Read-in the list of models
  models <- read_excel(paste0(path, gridInfo), "GCM_Lists", col_names = F) %>% pull(1)
  
  #Read-in grid-information for each model
  grid <- lapply(models, function(x) read_excel(paste0(path, gridInfo), x)) %>%
    setNames(models)
  
  #Loop through scenarios & gcms
  for (k in 1:length(scenarios)) {
    
    parent_dir <- paste0(path, scenarios[[k]])
    gcm_names  <- list.dirs(path = parent_dir, full.names = F, recursive = F)
    gcm_dirs   <- list.dirs(path = parent_dir, full.names = T, recursive = F)
    
    for (i in 1:length(gcm_dirs)) {
    
      grid_cur <- grid[[gcm_names[[i]]]] %>%
        mutate(coord = paste(formatC(Lat, format = 'f', flag='0', digits = 6),
          formatC(Lon, format = 'f', flag='0', digits = 6), sep = "_")) %>%
        select(coord, area = `Area(%)`) %>% mutate(area = area / 100)
    
      files <- dir(gcm_dirs[[i]], recursive = T, full.names = T)
      file_names <- dir(gcm_dirs[[i]], recursive = T, full.names = F)
      
      data <- lapply(files, function(x) read.table(x)) %>% 
        setNames(file_names) %>% bind_rows(.id = "coord") %>% 
        setNames(file_header) %>% as_tibble() %>%
        left_join(grid_cur, by = "coord")
      
      if(identical(climateVars, c("prcp", "tavg", "tmax", "tmin"))) {
        
        data2 <- data %>% 
          group_by(year, mon) %>%
          summarize(prcp = sum(prcp * area), tavg = sum(tavg * area), 
                    tmax = sum(tmax * area), tmin = sum(tmin * area)) %>%
          ungroup()

      } else {
        
        data2 <- data %>% 
          group_by(year, mon) %>%
          summarize(prcp = sum(prcp * area), tavg = sum(tavg * area)) %>%
          ungroup()
      }

      out[[scenarios[[k]]]][[gcm_names[[i]]]] <- data2
    }
  }
  return(out)
}


#gcm_data <- gcmDataTransform(path = "./data/climate projections/")
