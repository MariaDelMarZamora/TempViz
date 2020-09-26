#Reading in and compiling data

library(tidyverse)
library(data.table)
library(lubridate)
library(readr)

years_int <- c(1985:1990, 2000:2005) #set the relevant years for files

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

aTemp <- aTemp %>% 
  group_by(fest, fmeses) %>% 
  mutate(pre2000 = case_when(year < 2000 ~ "base",
                             year >= 2000 ~ "post")
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
    diff = temperature - avgTemp_base
  )

#----------------Visualization--------------------

#remove year factor() when importing all data
ggplot(aTemp, aes(x = fmeses, y = factor(year),  color = diff)) +
  geom_point(size = 3)+
  facet_wrap(vars(fest)) +
  scale_color_gradient2(low = "blue", high = "red")

