
################################################################################
# WRITE FILES (FOR WEAP) ++++++++++++++++++++++++++++++++++++++++++++++++++++++  

parent_dir <- "./input/weap-str-climate"
scn_dir <- 1:length(future_climate_rlz)

#Create climate scenario directories
sapply(scn_dir, function(x) dir.create(file.path(paste0(parent_dir,"/", x))))

#Loop through each scenario and create files
pb <- txtProgressBar(min = 1, max = clim_num, style = 3)

clim_header <- c("Year",	"Month",	"Precip (mm/mo)",	"Tmean (degC)",	"Tmin (degC)",	"Tmax (degC)")

for (k in 1:clim_num) {
  
  for(s in 1:loc_num) {
    
    #Subset climate data for the given catchment
    df <- future_climate_rlz[[k]] %>% filter(id == s) %>%
      select(year, month, prcp, tavg, tmin, tmax)
    
    colnames(df) <- clim_header
      
    #Save tp file
    cat("\n", file = paste0(parent_dir,"/",k,"/",loc_file[s]), append = TRUE) 
    cat("\n", file = paste0(parent_dir,"/",k,"/",loc_file[s]), append = TRUE) 
    #cat("\n", file = paste0(parent_dir,"/",k,"/",loc_file[s]), append = TRUE) 
    write_csv(x = df, path = paste0(parent_dir,"/",k,"/",loc_file[s]), 
              col_names = TRUE, append = TRUE)  
  }
  setTxtProgressBar(pb, k)
  
}
close(pb) 
