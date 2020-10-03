#Reading in and compiling data

library(tidyverse)
library(data.table)
library(lubridate)
library(readr)
library(gganimate)
library(RColorBrewer)
library(transformr)
library(tweenr)
#library(ggridges)


years_int <- c(1985:2019) #set the relevant years for files

# create file paths/names
file_names <-paste("Mex Temp/", years_int, "Tmed.xlsx", sep = "")


#d85<- readxl::read_xlsx("Mex Temp/1985Tmed.xlsx", skip = 1, 
#                        col_types = c("text", "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
#                                     "numeric","numeric","numeric","numeric","numeric","numeric")
#                         )


#read in xlsx files. Results in a list of dataframes
list_dfs <- lapply(file_names, readxl::read_xlsx, 
                   skip = 1,
                   col_types = c("text", "numeric","numeric","numeric","numeric","numeric","numeric","numeric",
                                 "numeric","numeric","numeric","numeric","numeric","numeric")
                  )


#Name each element in the list (year)
names(list_dfs) <- paste(years_int)

#unnest/bind data frames
aTemp <- bind_rows(list_dfs, .id = "year")

#Tidy data, need to result in a long df of state, year, month, temp
aTemp <- aTemp %>% 
  gather(key = "month", value = "temperature", ENE:ANUAL) %>% 
  mutate(year = as.integer(year(as.Date.character(year, "%Y"))) 
         )

meses <- unique(aTemp$month) #ordered levels of the months
estados <- unique(aTemp$ENTIDAD) # levels for states

#read in state classification possibilities
class_states <- readxl::read_xlsx("Mex Temp/states_class.xlsx",
                        col_names = c("estados", "turistica", "seguridad", "orden", "region")) %>% 
  arrange(orden) %>% 
  mutate(estados = toupper(estados),
         orden = as.integer(orden))

#mutate region to correct levels
class_levels <- as.character(unique(class_states$region)) #extracting correct order of levels

class_states <- class_states %>% 
  mutate(
    region = factor(region),
    region = fct_relevel(region, class_levels)
  )

# left join to bring in state classifications 
aTemp <- aTemp %>% left_join(class_states, by = c("ENTIDAD" = "estados")) 


#factor month var, with correct levels
aTemp<- aTemp %>% 
  mutate(fmeses = factor(month),
         fmeses = fct_relevel(fmeses, meses),
         num_month = as.numeric(fmeses),
         fest = factor(ENTIDAD, levels = estados),
         reg_state = paste(region, " : ", fest)
  ) %>% 
  select(year, fmeses, num_month, fest, temperature, region, orden, reg_state)%>% 
  arrange(orden)

name_levels <- unique(aTemp$reg_state)

aTemp <- aTemp %>% 
  mutate(
    reg_state = factor(reg_state),
    reg_state = fct_relevel(reg_state, name_levels)
  )



#calculate average temp before and after midpoint of available data (or year 2000)
#foreach state, each month, group by state, month, mean(temp)
#calculate difference between average and measured temp per state per month

aTemp <- aTemp %>% 
  group_by(fest, fmeses) %>% 
  mutate(pre2000 = case_when(year < 2000 ~ "base",
                             year >= 2000 ~ "post"),
         avgTemp_whole = mean(temperature)
         ) %>% 
  ungroup()


#extract baseline subset df and calculate group (state, month) means
base <- aTemp %>% 
  filter(year <2000) %>% 
  group_by(fest, fmeses) %>% 
  mutate(avgTemp_base = mean(temperature)
         ) %>% 
  select(fest, fmeses, avgTemp_base) %>% 
  unique()

#rejoin to main data set _ left join

aTemp <- aTemp %>% 
  left_join(base, by = c("fest","fmeses")) %>% 
  mutate(
    diff = temperature - avgTemp_base, #compared to average temps prior to 2000
    binned_diff = case_when(diff > mean(diff) - sd(diff) &
                              diff < mean(diff) + sd(diff) ~ 0,
                            diff > mean(diff) + sd(diff) &
                              diff < mean(diff) + 2*sd(diff) ~ 1,
                            diff > mean(diff) + 2*sd(diff) &
                              diff < mean(diff) + 3*sd(diff) ~ 2,
                            diff > mean(diff) + 3*sd(diff) ~ 3,
                            diff < mean(diff) - sd(diff) &
                              diff > mean(diff) -2*sd(diff) ~ -1,
                            diff < mean(diff) -2*sd(diff) &
                              diff > mean(diff) -3*sd(diff) ~ -2,
                            diff < mean(diff) -3*sd(diff) ~ -3
    ),
    diff_whole = temperature - avgTemp_whole #compared to average temps over the ENTIRE period
    #diff_85index = #indexed to 1985 temperatures
  )



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
  geom_point(size = 2, alpha = 0.8) +        #shape 0 is a hollow square
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
  shadow_wake(wake_length = 0.5, alpha = FALSE)
  #shadow_mark(size = 1, alpha = 0.4)

animate(p, fps = 2, renderer = gifski_renderer(), height = 8, width = 11, unit = "in", res = 200)  
anim_save("Temp_Mex_wake.gif")

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