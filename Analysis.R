library("tidyverse")
library(data.table)

# import data
file_names <- list.files(path = "Data/", pattern = ".csv", full.names = TRUE)
`tripdata_10-21_09-22` <- do.call(rbind, lapply(file_names, read_csv))

temp <- `tripdata_10-21_09-22` %>% 
  mutate("day_of_week" = weekdays(temp$ended_at))
