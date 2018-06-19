
## *****************************************************************************
## WAVELET AUTOREGRESSIVE MODEL (WARM)
## *****************************************************************************

## Load tidy climate data
#load("./input/hist_climate_data_ground.Rdata")
#load("./input/hist_climate_data_gridded.Rdata")

#-------------------------------------------------------------------------------
### 1) SPECTRAL ANALYSIS OF ANNUAL HISTORICAL PRECIP --------------------------

ANNUAL_PRCP <- hist_climate_avg_yr$prcp 

CLIMATE_VARIABLE <-  ANNUAL_PRCP
CURRENT_CLIMATE_VARIABLE_org <- CLIMATE_VARIABLE
hist_sim_length <- length(CLIMATE_VARIABLE)
 
output <- waveletAnalysis(variable = CLIMATE_VARIABLE, siglvl = 0.95)
list2env(output,globalenv())

#Power spectra of observed annual precip
POWER_SPECTRUM_PRCP_OBS <- GWS
#Significance level for observed annual precip
POWER_SPECTRUM_PRCP_SIGNIFICANCE <- signif_GWS
#Power spectra periods
POWER_SPECTRUM_PRCP_PERIOD <- period

# p <- ggWaveletSpectra(period = period, sig = signif_GWS, obs = GWS) +
#   scale_x_continuous(breaks=seq(5,45,10), limits = c(0, 50), expand=c(0,0)) +
#   scale_y_log10(labels = comma, breaks = c(1, 10, 20, 50, 100, 250, 500, 1000) * 10^3) +
#   theme(panel.grid.minor = element_blank())
# 
# ggsave("./graphics/climate-chr/ground/prcp_annual_spectral_obs.png", height = 6, width = 6)

# ------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
### 2) GENERATE NEW ANNUAL PRECIP SERIES USING WARM ----------------------------

#Arrays to store simulated series
PRCP_FINAL_ANNUAL_SIM             <- array(NA,c(sim_length,sim_num))
PRCP_FINAL_ANNUAL_SIM_TRANSFORMED <- array(NA,c(sim_length,sim_num))

#Define an array specifying low frequency components
LOW_FREQUENCY_COMPONENTS <- array(0,c(length(CLIMATE_VARIABLE),component_num))

#Loop through each each ortogonal component series
for (i in 1:component_num) {
  
  #Specify periods in the current component
  CUR_PERIODS <- ALL_SIG_PERIODS[1:NUM_PERIODS_ALL_COMPS[i]]
  #If there is more than one period:
  if (i>1) {
    CUR_PERIODS <- ALL_SIG_PERIODS[(1 + (i-1)*NUM_PERIODS_ALL_COMPS[i-1]):(NUM_PERIODS_ALL_COMPS[i] + (i-1)*NUM_PERIODS_ALL_COMPS[i-1])]
  }
  sj <- scale[CUR_PERIODS]
  
  #for Morlet Wavelet with freq = 6
  Cdelta <- .776
  w0_0 <- pi^(-1/4)
  
  if (length(CUR_PERIODS) > 1) {
    LOW_FREQUENCY_COMPONENTS[,i] <- apply(sd(CURRENT_CLIMATE_VARIABLE_org)*(dj*sqrt(dt)/(Cdelta*w0_0))*Re(wave)[CUR_PERIODS,]/sqrt(sj), 2, sum)
  }
  
  if (length(CUR_PERIODS) == 1) {
    LOW_FREQUENCY_COMPONENTS[,i] <- sd(CURRENT_CLIMATE_VARIABLE_org)*(dj*sqrt(dt)/(Cdelta*w0_0))*Re(wave)[CUR_PERIODS,]/sqrt(sj)
  }
    
}
NOISE <- CURRENT_CLIMATE_VARIABLE_org - apply(LOW_FREQUENCY_COMPONENTS, 1, sum)
PRCP_LOW_FREQUENCY_COMPONENTS <- LOW_FREQUENCY_COMPONENTS
PRCP_NOISE <- NOISE

###### SIMULATE NEW SERIES USING WAVELET ARIMA-MODEL
pb <- txtProgressBar(min = 1, max = sim_num, style = 3)
for (y in 1:sim_num) {
  
  #Simulate the noise component using an ARIMA model 
  PRCP_NOISE_MEAN <- mean(PRCP_NOISE)
  PRCP_NOISE_CENTERED  <- PRCP_NOISE - PRCP_NOISE_MEAN
  MODEL1 <- auto.arima(PRCP_NOISE_CENTERED, max.p=5, max.q=0, max.P = 0, max.Q = 0, stationary = TRUE)
  
  #Intercept of the arima model
  AR1 <- as.vector(MODEL1$coef)[which(names(MODEL1$coef)!="intercept")]
  INTERCEPT <- 0
  
  #If the model is higher order 
  if (length(which(names(MODEL1$coef)=="intercept"))>0) {
    INTERCEPT <- as.vector(MODEL1$coef)[which(names(MODEL1$coef)=="intercept")]
  }
  
  #Simulate noise component
  SIM1 <- arima.sim(n = (sim_length+1), list(ar = AR1), sd = sd(residuals(MODEL1))) 
  SIM1 <- SIM1[2:(sim_length+1)] + INTERCEPT + PRCP_NOISE_MEAN
  
  #Simulate low frequency component(s)
  SIM2 <- array(NA,c(sim_length,component_num))
  
  for (k in 1:component_num) {
    
    PRCP_LOW_FREQUENCY_COMPONENTS_MEAN <- mean(PRCP_LOW_FREQUENCY_COMPONENTS[,k])
    PRCP_LOW_FREQUENCY_COMPONENTS_CENTERED  <- PRCP_LOW_FREQUENCY_COMPONENTS[,k] - PRCP_LOW_FREQUENCY_COMPONENTS_MEAN
    
    MODEL2 <- auto.arima(PRCP_LOW_FREQUENCY_COMPONENTS_CENTERED,max.p=5,max.q=0,max.P=0,max.Q=0,stationary=TRUE)
    AR2 <- as.vector(MODEL2$coef)[which(names(MODEL2$coef)!="intercept")]
    INTERCEPT <- 0
    
    if (length(which(names(MODEL2$coef)=="intercept"))>0)
    {INTERCEPT <- as.vector(MODEL2$coef)[which(names(MODEL2$coef)=="intercept")]}
    SIM2[,k] <- arima.sim(n = sim_length, list(ar = AR2), sd = sd(residuals(MODEL2)))
    SIM2[,k] <- SIM2[,k] + INTERCEPT + PRCP_LOW_FREQUENCY_COMPONENTS_MEAN
  }
  
  #Aggregate all components and obtain the simulated time-series
  PRCP_FINAL_ANNUAL_SIM[,y] <- rowSums(cbind(SIM1,SIM2))

  #Inverse transform
  #PRCP_FINAL_ANNUAL_SIM[,y] <- InverseBoxCox(lambda,PRCP_FINAL_ANNUAL_SIM[,y])
  
  #Y <- PRCP_FINAL_ANNUAL_SIM[,y]
  #result <- (lambda*Y + 1)  ^ (1/lambda)
  
  setTxtProgressBar(pb, y)
}
close(pb)

###### Wavelet spectra of simulated ARIMA time-series 
GWS_length <- length(POWER_SPECTRUM_PRCP_OBS) + 1
POWER_SPECTRUM_PRCP_ARIMA_SIM <- array(NA,c(length(GWS),sim_num)) 
for (y in 1:sim_num) {

  #Wavelet spectra for the simulated time-series
  WAVELET_VARIABLE <- PRCP_FINAL_ANNUAL_SIM[,y]
  CLIMATE_VARIABLE <- WAVELET_VARIABLE
  
  output <- waveletAnalysis(variable = CLIMATE_VARIABLE)
  list2env(output,globalenv())
  POWER_SPECTRUM_PRCP_ARIMA_SIM[,y] <- GWS
  
  setTxtProgressBar(pb, y)
}
close(pb)

# Check wavelet spectra of simulated time-series
p <- ggWaveletSpectra(period = period, sig = signif_GWS, obs = GWS, 
    sim = POWER_SPECTRUM_PRCP_ARIMA_SIM) +
  scale_x_continuous(breaks=seq(5,45,10), limits = c(0, 50), expand=c(0,0)) +
  scale_y_log10() +
  theme(panel.grid.minor = element_blank())
ggsave("./graphics/wegen/prcp_annual_spectral_sim_init.png", height = 6, width = 6)

# ------------------------------------------------------------------------------

