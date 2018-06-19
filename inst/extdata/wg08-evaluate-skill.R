


future_climate_rlz <- read_rds("./input/future_climate_rlz.rds")
climate_vars_output <- c("prcp", "tavg", "tmin", "tmax")

future_climate_rlz_avg <- future_climate_rlz %>% 
  bind_rows(.id = "var_id") %>%
  mutate(area = loc_area[as.numeric(id)]) %>%
  mutate_at(vars(climate_vars_output), funs(tempf(., area))) %>%
  mutate(id = 1) %>%
  group_by(var_id, year, month, day) %>%
  summarize_at(vars(climate_vars_output), funs(sum)) %>%
  arrange(var_id, year, month, day)
  
#-------------------------------------------------------------------------------
### 5) EVALUATE MODEL SKILL ----------------------------------------------------

#### 3) Monthly box-plots of daily average temp and precip 
hist_climate_rlz_avg <- hist_climate_rlz %>%
  bind_rows(.id = "index") %>%
  mutate(id = as.numeric(id)) %>%
  mutate(prcp = prcp * loc_area[id]/100, 
         tavg = tavg * loc_area[id]/100) %>%
  group_by(index, year, month, day) %>%
  summarize(prcp = sum(prcp), tavg = sum(tavg))  

clim_rlz_seas_means <- hist_climate_rlz_avg %>% 
  group_by(index, month) %>%
  summarize(prcp = mean(prcp), tavg = mean(tavg)) %>%
  mutate(month = factor(month, levels = 1:12, labels = month.abb))

hist_clim_seas_means <- hist_climate_avg %>%
  group_by(month) %>%
  summarize(prcp = mean(prcp), tavg = mean(tavg))

p3a <- ggplot(clim_rlz_seas_means, aes(x=month, y = prcp)) +
  geom_boxplot(outlier.size = 0.3) +
  #geom_quasirandom(alpha = 0.4) +
  geom_point(data = hist_clim_seas_means, color = "blue", shape = 16, size = 1.8) +
  labs(x = "", y = "mm/day")

p3b <- ggplot(clim_rlz_seas_means, aes(x=month, y = tavg)) +
  geom_boxplot(outlier.size = 0.3) +
  geom_point(data = hist_clim_seas_means, color = "blue", shape = 16, size = 1.8) +
  labs(x = "", y = expression(""*degree*C*""))

p3 <- cowplot::plot_grid(p3a, p3b, nrow =2, labels = c("a)", "b)"), label_size = 11)
ggsave(paste0(plot_dir,"/sim_traces_skill_boxplot.png"), height = 4, width = 8)

#-------------------------------------------------------------------------------

