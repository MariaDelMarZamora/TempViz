source("DataProc.R")

#----------------Visualization--------------------

#point, semi heat map
#remove year factor() when importing all data
ggplot(aTemp, aes(x = fmeses, y = factor(year),  color = diff)) +
  geom_point()+
  facet_wrap(vars(fest)) +
  scale_color_gradient2(low = "blue", high = "red")


#spaghetti plots

# -- yearly plots, could play with color, lower alpha
#what about making a var that groups five year chunks together, so although
# the spaghetti colors would be "grouped"
ggplot(aTemp, aes(num_month, y = temperature, color = factor(year))) +
  geom_line(alpha = 0.4) +
  #  scale_color_brewer(type = "seq", palette = "Spectral") +
  facet_wrap(vars(fest), scales = "free_y")


#--- temp v year, group(color) line
ggplot(aTemp, aes(year, temperature, color = fmeses)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = F)+
  geom_smooth(color = "blue", size = 1)+
  facet_wrap(vars(fest), scales = "free_y")

# animated.

p <- ggplot(aTemp, aes(fmeses, temperature, color = as.factor(binned_diff))) +
  #geom_smooth(size = 1, se = F, alpha =  0.05) +
  geom_point(size = 4, alpha = 0.8) +        #shape 0 is a hollow square
  facet_wrap(vars(reg_state), nrow = 7, ncol = 5, scales = "free_y") +         #, scales = "free_y"
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  geom_vline(xintercept = 12.5, linetype = "dashed") +
  #theme_light() +
  theme(
    panel.grid.major.x = element_blank(), 
    axis.text.x = element_text(angle = 45,
                               size = 10,
                               hjust = 1),
    axis.title.x = element_blank(),
    
  ) +
  guides(color = "none") +
  labs( x = "Mes",
        y = "Temperatura promedio",
        title = "Temperatura promedio por entidad entre 1985-2019",
        subtitle = "Year: {frame_time}"
  ) +
  transition_time(year) +
  shadow_wake(wake_length = 1, alpha = FALSE)
#shadow_mark(size = 1, alpha = 0.4)

animate(p, fps = 2, renderer = gifski_renderer(), height = 8, width = 11, unit = "in", res = 200)  
anim_save("Temp_Mex_wake2.gif")

ggplot(aTemp, aes(year, diff, color = fmeses)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = F, method = "lm")+
  geom_smooth(color = "blue", size = 1, method = "lm")+
  facet_wrap(vars(fest))


# --------------- RIDGES --------------
# 
# nacional_temp <-aTemp %>% 
#   filter(fest == "NACIONAL" ) 
# 
# 
# ggplot(nacional_temp, aes(fmeses, y = rep(1, 455), # need to understand the y aesthetic
#                           height = temperature, 
#                           group = year, 
#                           color = year))+
#   geom_ridgeline(alpha = 0.05, fill = "lightblue")
# 
# #could try artificially generating the data, with one observation corresponding to one degree (or tenth of a degree?)
# #of temperature, so the height of the kernel corresponds to the temperature
# 
# 
# test_df <- data.frame(x = 1:12, y = rep(1,10), temp = c(10,13,))