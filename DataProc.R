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
class_states <- readxl::read_xlsx("Mex Temp/states_class.xlsx", skip = 1,
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



