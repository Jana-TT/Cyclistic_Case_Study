
##PROCESS phase: 
## have to split the data into date and time into separate columns
library(tidyverse)
data_202207_new <- 
  data_202207 %>% 
  separate(started_at, into=c("start_date", "start_time"), sep = " ") %>% 
  separate(ended_at, into=c("end_date", "end_time"), sep = " ")

## converting the times from chr to num
library(lubridate)
data_202207_new$start_time <- hms::as_hms(data_202207_new$start_time) 
data_202207_new$end_time <- hms::as_hms(data_202207_new$end_time) 

##creating the new column for the total time riders rode
library(dplyr)
data_time <-
  data_202207_new %>% 
  mutate(data_202207_new, ride_length = end_time - start_time) %>% 
  ##converts the date from chr to type Date
  data_time$start_date <- as.POSIXlt(data_202207_new$start_date) 

##added a column day_of_week, for the day of the week depending on the date
data_time <- mutate(data_time, day_of_week = weekdays(data_time$start_date))

## my way of doing it!

#ANALYZE phase:

##comparing column names for each of the files. While names don't have to be in the same order, they DO need to 
## before we can use a command to join them into one file
colnames(data_202207)
colnames(data_202208)
colnames(data_202209)
colnames(data_202210)
colnames(data_202211)
colnames(data_202212)
colnames(data_202301)
colnames(data_202302)
colnames(data_202303)
colnames(data_202304)
colnames(data_202305)
colnames(data_202306)

##combining all the data frames into ONE big data frame
all_cycle_trips <- bind_rows(data_202207, data_202208, data_202209, data_202210, data_202211, data_202212, data_202301, data_202302, data_202303, data_202304, data_202305, data_202306)

##removing info that doesn't help us 
all_cycle_trips <- 
  all_cycle_trips %>% 
  select(-c(start_lat, start_lng, end_lat, end_lng))

##inspecting the new table that has been created
colnames(all_cycle_trips)
nrow(all_cycle_trips)
dim(all_cycle_trips)
head(all_cycle_trips)
str(all_cycle_trips)
summary(all_cycle_trips) ##statistical summary of the data. Mainly for numerics

##separates the original date in the able in month, day, year, day_of_week, and 
##calculates the ride_length
##https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_cycle_trips$date <- as.Date(all_cycle_trips$started_at) #The default format is yyyy-mm-dd
all_cycle_trips$month <- format(as.Date(all_cycle_trips$date), "%m")
all_cycle_trips$day <- format(as.Date(all_cycle_trips$date), "%d")
all_cycle_trips$year <- format(as.Date(all_cycle_trips$date), "%Y")
all_cycle_trips$day_of_week <- format(as.Date(all_cycle_trips$date), "%A")
all_cycle_trips$ride_length <- difftime(all_cycle_trips$ended_at,all_cycle_trips$started_at)

##converting ride_length from factor to numeric to we can run calculations on the data
str(all_cycle_trips)
all_cycle_trips$ride_length <- as.numeric(as.character(all_cycle_trips$ride_length))
is.numeric(all_cycle_trips$ride_length)

##removing bad data
##The data frame includes a few hundred entries when bikes were taken out of docks 
##and checked for quality by Divvy or ride_length was negative
### We will create a new version of the dataframe (v2) since data is being removed
all_cycle_trips_v2 <- all_cycle_trips[!(all_cycle_trips$start_station_name == "HQ QR" | all_cycle_trips$ride_length<0),]

#descriptive analysis on ride_length
mean(all_cycle_trips_v2$ride_length)
median(all_cycle_trips_v2$ride_length)
max(all_cycle_trips_v2$ride_length)
min(all_cycle_trips_v2$ride_length)

##you can condense this above ^^^ to one line using summary() function
summary(all_cycle_trips_v2$ride_length)

##comparing members and casual users
aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual, FUN = mean)
aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual, FUN = median)
aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual, FUN = max)
aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual, FUN = min)

##average ride time by each day for members vs casual users
aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual + all_cycle_trips_v2$day_of_week, FUN = mean)

##sorting the days of the week properly 
all_cycle_trips_v2$day_of_week <- ordered(all_cycle_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual + all_cycle_trips_v2$day_of_week, FUN = mean)

##analyzing ridership data by type and weekday
library(dplyr)
library(lubridate)
all_cycle_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by user type and weekday
  summarise(number_of_rides = n()				#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		#calculates the average duration
  arrange(member_casual, weekday)								#sorts

##Let's visualize the number of rides by rider type
library(ggplot2)
all_cycle_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = format(n(), scientific = FALSE) 
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of Rides by Rider Type", x = "Days of the Week", y = "Number of Rides", caption = "Data made available by Motivate International Inc.") 
 
  

##Visualizing for average duration
all_cycle_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


counts <- aggregate(all_cycle_trips_v2$ride_length ~ all_cycle_trips_v2$member_casual + all_cycle_trips_v2$day_of_week, FUN = mean)

write.csv(counts, file = "C:\\Users\\janat\\Downloads\\counts.csv")


