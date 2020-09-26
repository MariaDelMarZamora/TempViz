#Reading in and compiling data

library(tidyverse)
library(data.table)
library(lubridate)
library(readr)

years_int <- 1985:1990 #set the relevant years for files

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
  mutate(year = year(as.Date.character(year, "%Y")))

meses <- unique(aTemp$month) #ordered levels of the months
estados <- unique(aTemp$ENTIDAD) # levels for states

#factor month var, with correct levels
aTemp <- aTemp %>% 
  mutate(fmeses = factor(month),
         fmeses = fct_relevel(fmeses, meses),
         fest = factor(ENTIDAD, levels = estados)) %>% 
  select(year, fmeses, fest, temperature)



#calculate average temp before and after midpoint of available data (or year 2000)
#foreach state, each month, group by state, month, mean(temp)
#calculate difference between average and measured temp per state per month

temp_calc <- aTemp %>% 
  group_by(fest, fmeses) %>% 
  mutate(pre2000 = case_when(year < 2000 ~ "base",
                             year >= 2000 ~ "post"),
         avgTemp_base =mean(temp_calc$temperature[temp_calc$pre2000 == "base"]),
         diff = temperature - avgTemp_base
         )

mean(temp_calc$temperature[temp_calc$pre2000 == "base"])


ggplot(aTemp, aes(x = fest, y = 1, size = temperature, color = temperature)) +
  geom_point()+
  facet_wrap(vars(fest))+
  scale_fill_gradient(low = "blue", high = "red")

