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

