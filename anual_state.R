#Anual per state

source("DataProc.R")


# keep only annual data (drop monthly)
aTemp <- aTemp %>%
  filter(fmeses == "ANUAL") %>% 
  mutate(
    round_diff = round(diff, digits = 3),
    round_temp = round(temperature, digits = 2)
  )

# base for animation
p <-
  ggplot(aTemp, aes(x = fest, y = 1, color = as.factor(binned_diff)))+
  geom_point(size = 20) +
  geom_text(aes(label = as.character(round_temp)), color = "black")+
  scale_color_brewer(palette = "Spectral", direction = -1) +
  facet_wrap(vars(reg_state), ncol = 5, nrow = 7, scales = "free_x")+
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom"
    )+
  guides(color = "none") +
  labs( title = "Temperatura promedio anual por entidad entre 1985-2019",
        subtitle = "Year: {frame_time}",
        caption = "Colores representan desviaciones estandard de la temperatura promedio entre 1984-1999"
        # ,
        # color = "DS de promedio"
  ) +
  transition_time(year)

animate(p, fps = 3, width =450, height = 400)
anim_save("Anual_1.gif")
