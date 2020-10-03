#Anual per state

library(tidyverse)
library(ggplot2)
library(gganimate)



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