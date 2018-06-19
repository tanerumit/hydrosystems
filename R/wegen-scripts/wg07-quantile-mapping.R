
# ------------------------------------------------------------------------------
# QUANTILE MAPPING ----------------------------------------------------------

# Daily, multi-site, multi-variable synthetic climate realizations
clim_rlz_full <- read_rds(path = "./input/stoc_clim_rlz.rds")
clim_rlz <- clim_rlz_full[subset] #select only first two

#Calculate coefficient of variation of daily prcp (CV) for each realization
cv_adj  <- c(0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4)
cv_adj_labs <- paste0((cv_adj-1)*100, "%")
cv_adj_cols <- RColorBrewer::brewer.pal(n= length(cv_adj), name="Blues")

#Indices   
cv_num  <- length(cv_adj)
rlz_num <- length(clim_rlz)
loc_num <- length(unique(clim_rlz[[1]]$hru_id))

# Fit historical climate data to a theoretical distribution
base_dist <- list()
base_dist$fit  <- vector(mode = "list",length = 12)
base_dist$type <- "gamma"   # log-normal
 
# Define target distribution for quantile mapping
targ_dist <- list()
targ_dist$fit <- vector(mode = "list",length = 12)
targ_dist$type <- "gamma" # log-normal

#Nested lists from the most outer: hrus/realizations/cv-mod
list_lev1 <- vector(mode = "list", length = cv_num)
list_lev2 <- rep(list(list_lev1), rlz_num) 
list_lev3 <- rep(list(list_lev2), loc_num)
future_clim_init <- list_lev3

#r  <- climate realization index
#h  <- hru index
#v  <- variance modif index
#cc <- climate change modif index

#Loop through each climate realization
for (r in 1:rlz_num) {

  #Select current climate realization
  clim_rlz_curr <- clim_rlz[[r]]
  
  for (h in 1:loc_num) {
  
    df <- list()
    
    #Select current hru 
    clim_rlz_hru_curr <- clim_rlz_curr %>% filter(hru_id == h) %>% 
      dplyr::select(-hru_id)
      
    #Loop through each month
    for (m in 1:12) {
    
      # Select prcp in given month  (filter nonzero & unique values)
      prcp_m <- clim_rlz_hru_curr %>% 
        filter(month == m) %>% filter(prcp > 0) %>% pull(prcp) %>% unique(.)
      
      # Fit prcp data to the specied distribution 
      base_dist$fit[[m]]  <- fitdistrplus::fitdist(data=prcp_m, distr="gamma") 
      # Shape parameter
      base_dist$par1[[m]] <- base_dist$fit[[m]]$estimate[[1]]
      # Scale parameter
      base_dist$par2[[m]] <- 1/base_dist$fit[[m]]$estimate[[2]]
      #Plot goodness of fit
      df[[m]] <- data_frame(x = prcp_m) %>% 
        mutate(x = sort(x, na.last = T), qtile = (1:n())/(n()+1)) %>%
        mutate(y = qgamma(qtile, shape = base_dist$par1[[m]], scale = base_dist$par2[[m]]))
        
    }
    
    #Check goodness of fit
    df2 <- bind_rows(df, .id = "month") %>%
    mutate(month = as.numeric(month),
           month = factor(month, levels = 1:12, labels = month.abb))
    
    p <- ggplot(df2, aes(x,y)) + 
    ggtitle("Observed vs estimated precip (from gamma dist.)") +
    theme_bw() +
    facet_wrap( ~ month, nrow = 4) +
    geom_abline(intercept = 0, slope = 1, color = "blue") +
    geom_point(alpha = 0.7, shape = 1, size = 2) + 
    labs(x = "observed (mm/day)", y = "estimated (mm/day)") 
    
    pname2 <- paste0("rlz_",r,"_hru_",h,"_gamma_fit.png")
    #ggsave(paste0(pname, pname2), height = 10, width = 8)
    
    #Check stats 
    #Kolmogorov-Smirnov Test
    #ks <- lapply(1:12, function(m) 
    #ks.test(df[[m]]$x, "pgamma", base_dist$par1[[m]], 1/base_dist$par2[[m]], exact = NULL))
    
    #Chi-square test
    #chi <- lapply(1:12, function(m) chisq.test(df[[m]]$x[100:200], df[[m]]$y[100:200]))
    
    #Define base and target distribution parameters 
    #gamma distribution: mean = shape * scale, variance = shape * scale^2
    base_dist$mean <- base_dist$par1 * base_dist$par2
    base_dist$sd   <- sqrt(base_dist$par1 * (base_dist$par2)^2)
    base_dist$cv   <- base_dist$sd / base_dist$mean
    base_dist$var  <- base_dist$sd^2
    
    # List of parameters for EACH variance level  
    targ_dist$mean <- lapply(1:cv_num, function(x) base_dist$mean)
    targ_dist$cv   <- lapply(1:cv_num, function(x) base_dist$cv * cv_adj[x])
    targ_dist$sd   <- lapply(1:cv_num, function(x) targ_dist$cv[[x]] * targ_dist$mean[[x]])
    targ_dist$var  <- lapply(1:cv_num, function(x) targ_dist$sd[[x]] ^ 2)
    
    #par2: scale parameter / par1: shape parameter
    targ_dist$par2 <- lapply(1:cv_num, function(x) targ_dist$var[[x]] / targ_dist$mean[[x]])
    targ_dist$par1 <- lapply(1:cv_num, function(x) targ_dist$mean[[x]] / targ_dist$par2[[x]]) 
    
    clim_rlz_hru_cv_curr <- list()
    
    #Loop through variance scenarios
    for (v in 1:cv_num) {
    
      #Select current cv modification
      clim_rlz_hru_cv_curr[[v]] <- clim_rlz_hru_curr %>% mutate(quantile=NA, new=NA)
      
      #Loop through months
      for (m in 1:12) {
      
        # Find quantiles of baseline precip
        clim_rlz_hru_cv_curr[[v]]  %<>%
          mutate(quantile = ifelse(month == m, 
            pgamma(prcp, shape = base_dist$par1[m], scale = base_dist$par2[m]), quantile)) 
            
        #Find new prcp based on specified quantiles
        clim_rlz_hru_cv_curr[[v]] %<>% 
          mutate(new = ifelse(month == m, 
            qgamma(quantile, shape = targ_dist$par1[[v]][m], scale = targ_dist$par2[[v]][m]), new)) 
      
    }
      
      #Save output data frame to the main list
      future_clim_init[[h]][[r]][[v]] <- clim_rlz_hru_cv_curr[[v]] %>% 
        dplyr::select(year, month, day, quantile, prcp = new, tavg, tmax, tmin)
    
    }
    
    ### PLOT CDFs for modified realizations 
    df <- future_clim_init[[h]][[r]] %>% bind_rows(.id = "var_id") %>%
      dplyr::select(variable = var_id, month, quantile, prcp) %>%
      mutate(variable = factor(variable, levels=1:cv_num, labels = cv_adj_labs)) %>%
      mutate(month = factor(month, levels = 1:12, labels = month.abb))
    
    p <- ggplot(df, aes(x = quantile, y = prcp)) +
    geom_line(aes(color = variable, group = variable), size = 0.8) +
    facet_wrap( ~ month, nrow = 4) +
    theme(plot.title = element_text(margin = margin(b = -10))) +
    scale_y_log10(limits = c(NA, 250), breaks = c(0.01, 0.1, 1, 10, 100)) +
    scale_x_continuous(breaks = seq(0, 1, 0.25)) +
    ylab("mm/day") + xlab("Non-exceedance (%)") +
    scale_color_manual(values = cv_adj_cols,labels = cv_adj_labs) +
    labs(color = "CV")
    
    pname2 <- paste0("rlz_",r,"_hru_",h,"_cdfs.png")
    #ggsave(paste0(pname, pname2), height = 10, width = 8)

  }

}

write_rds(x = future_clim_init, path = "./input/future_clim_init.rds", compress = "gz")

# ------------------------------------------------------------------------------



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################



memory.limit(size=56000)
rm(list=ls()); gc()
load(file = "./input/Rdata/wegen-data-04032018.RData")


########### Plot realizations

no_cc <- dplyr::filter(clim_mat, cv == 1 & prcp == 1 & tavg == 0) %>% pull(id)

clim_baseline_rlz <- lapply(no_cc, function(x) future_clim[[x]][[2]]) %>%
  bind_rows(.id = "id") 

df <- clim_baseline_rlz %>%
  group_by(id, year, month) %>%
  summarize(tavg = mean(tavg), prcp = sum(prcp)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "/")))  

df2 <- hist_clim_mon %>% filter(hru_id == 2) %>%
  ungroup() %>%
  mutate(year = df %>% filter(id == 1) %>% pull(year), 
        month = df %>% filter(id == 1) %>% pull(month)) %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "/")))  

p1 <- ggplot(df, aes(x = date, y = tavg)) + 
  theme_light() +
  geom_line(aes(group = id, color = id)) +
  scale_color_brewer(palette = "Dark2") +
  geom_line(data = df2, color = "black", size = 0.7) +
  labs(x = "", y = expression("("*degree*C*")"), color = "realization")

p2 <- ggplot(df, aes(x = date, y = prcp)) + 
  theme_light() +
  geom_line(aes(group = id, color = id)) +  
  scale_color_brewer(palette = "Dark2") +
  geom_line(data = df2, color = "black", size = 0.7) +
  labs(x = "", y = "mm/month", color = "realization") 

p <- cowplot::plot_grid(p1, p2, nrow = 2, labels = c("a)", "b)"))

ggsave(plot = p, filename = "rlz_monthly.png", height = 10, width = 8)


######### CHECK DAILY VALUES

prcp_cc <- dplyr::filter(clim_mat, cv == 1 & tavg == 0, rlz == 1) %>% pull(id)
prcp_labs <- seq(-40,40,10) 
  
clim_cc_prcp <- lapply(prcp_cc, function(x) future_clim[[x]][[2]]) %>%
  bind_rows(.id = "id") 

df <- clim_cc_prcp %>%
  group_by(id, year) %>%
  summarize(tavg = mean(tavg), prcp = sum(prcp)) %>%
  ungroup() %>%
  mutate(id = factor(id, levels = 1:9, labels = prcp_labs)) %>%
  filter(id %in% prcp_labs[c(1,3,5,7,9)])  
    
p2 <- ggplot(df, aes(x = year, y = prcp)) + 
  theme_light() +
  geom_line(aes(group = id, color = id)) +  
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = "mm/month", color = "precipitation \nchange (%)") 

tavg_cc <- dplyr::filter(clim_mat, cv == 1 & prcp == 1, rlz == 1) %>% pull(id)

clim_cc_tavg <- lapply(tavg_cc, function(x) future_clim[[x]][[2]]) %>%
  bind_rows(.id = "id") 

tavg_labs <- paste0("+",1:9)

df <- clim_cc_tavg %>%
  group_by(id, year) %>%
  summarize(tavg = mean(tavg), prcp = sum(prcp)) %>%
  ungroup() %>%
  mutate(id = factor(id, levels = 1:9, labels = tavg_labs)) %>%
  filter(id %in% tavg_labs[c(1,3,5,7,9)])  
    

p1 <- ggplot(df, aes(x = year, y = tavg)) + 
  theme_light() +
  geom_line(aes(group = id, color = id)) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = expression("("*degree*C*")"), color = "temperature\nincrease\n(DegC)")

p <- cowplot::plot_grid(p1, p2, nrow = 2, labels = c("a)", "b)"))

ggsave(plot = p, filename = "rlz_cc.png", height = 10, width = 8)




df <- clim_cc_prcp_means


p1 <- ggplot(df, aes(x = month, y = prcp, color = id)) +
 geom_boxplot() +
 theme_light() +
 labs(x = "", y = "Precipitation (mm/month)")
 
p2 <- ggplot(df, aes(x = month, y = tavg, color = id)) +
  geom_boxplot() +
  theme_light() +
  labs(x = "", y = expression("Temperature ("*degree*C*")"))

p <- cowplot::plot_grid(p1, p2, labels = c("A)", "B)"), nrow = 2)

ggsave(plot = p, filename = "./graphics/wegen/monthly_clim_realizations.png", width = 8, height = 10)


###############################################################################


# Mean changes 

# Temp
test <- filter(clim_mat, cv == 1 & prcp == 1 & rlz == 1) %>% pull(id)
sapply(test, function(x) mean(future_clim[[x]][[1]]$tavg))

# Precip
test <- filter(clim_mat, cv == 1 & tavg == 1 & rlz == 1) %>% pull(id)
sapply(test, function(x) mean(future_clim[[x]][[1]]$prcp)) 

# CV
test <- filter(clim_mat, prcp == 1 & tavg == 0 & rlz == 1) %>% pull(id)
mean <- lapply(test, function(x) mean(future_clim[[x]][[1]]$prcp)) 
sd   <- lapply(test, function(x) sd(future_clim[[x]][[1]]$prcp)) 
unlist(sd)/unlist(mean)

# Realizations
test <- filter(clim_mat, cv == 1 & prcp == 0.7 & tavg == 6) %>% pull(id)
sapply(test, function(x) mean(future_clim[[x]][[1]]$tavg))
sapply(test, function(x) sd(future_clim[[x]][[1]]$tavg))  

################################################################################


