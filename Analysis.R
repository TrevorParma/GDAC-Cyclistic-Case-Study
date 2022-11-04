library("tidyverse")
library(data.table)
library(lubridate)
library(ggplot2)

# Prepare
# import data into one dataframe
file_names <- list.files(path = "Data/", pattern = ".csv", full.names = TRUE)
tripdata <- do.call(rbind, lapply(file_names, read_csv))

# detach data.table (which I used to import all the data at once) because it causes problems later with lubridate.
detach("package:data.table", unload = TRUE)

# Process
# remove the unnecessary columns for latitude and longitude
tripdata <- tripdata %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

# add columns for the date, month, day, year, and day of the week of each ride
tripdata$date <- as.Date(tripdata$started_at)
tripdata$month <- format(as.Date(tripdata$date), "%m")
tripdata$day <- format(as.Date(tripdata$date), "%d")
tripdata$year <- format(as.Date(tripdata$date), "%Y")
tripdata$day_of_week <- format(as.Date(tripdata$date), "%A")

# sort days of the week
tripdata_2$day_of_week <- ordered(tripdata_2$day_of_week,
                                  levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# add a column for the ride length
tripdata$ride_length <- difftime(tripdata$ended_at, tripdata$started_at)
str(tripdata)

# change the type of the ride length column to numeric for calculations
is.factor(tripdata$ride_length)
tripdata$ride_length <- as.numeric(as.character(tripdata$ride_length))
is.numeric(tripdata$ride_length)
str(tripdata)

# ride length shouldn't be negative, so I make sure there aren't any instances of that
tripdata_2 <- tripdata[!(tripdata$ride_length < 0),]

# Analysis
# descriptive analysis on ride_length
mean(tripdata_2$ride_length)
median(tripdata_2$ride_length)
max(tripdata_2$ride_length)
min(tripdata_2$ride_length)

# compare members and casual riders
aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual, FUN = mean)
aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual, FUN = median)
aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual, FUN = max)
aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual, FUN = min)

# average ride length by each day for members vs casual users
aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual + tripdata_2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
tripdata_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)

# visualize the number of rides by rider type
tripdata_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# visualize average duration
tripdata_2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

# export csv files for creating visualizations
avg_ride_length_table <- aggregate(tripdata_2$ride_length ~ tripdata_2$member_casual +
                      tripdata_2$day_of_week, FUN = mean)
write.csv(avg_ride_length_table, file = "D:/Divvy_Analysis/avg_ride_length.csv")

number_of_rides_table <- group_by(tripdata_2, member_casual, day_of_week) %>% 
  summarise(number_of_rides = n())
write.csv(number_of_rides_table, file = "D:/Divvy_Analysis/number_of_rides.csv")
