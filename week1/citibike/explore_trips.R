library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)
<<<<<<< HEAD
NROW(trips) #5370361

# find the earliest and latest birth years (see help for max and min to deal with NAs)
max(trips$birth_year, na.rm=TRUE) #1998
min(trips$birth_year, na.rm=TRUE) #1899

# use filter and grepl to find all trips that either start or end on broadway
broadway_trips <- filter(trips, grepl('Broadway', end_station_name) | grepl('Broadway', start_station_name))

# do the same, but find all trips that both start and end on broadway
broadway_both <- filter(trips, grepl('Broadway', end_station_name) , grepl('Broadway', start_station_name))

# use filter, select, and distinct to find all unique station names
unique_start_stations <- trips %>% distinct(start_station_name) %>% select(start_station_name)
unique_end_stations <- trips %>% distinct(end_station_name) %>% select(end_station_name)
NROW(unique_start_stations) == NROW(unique_end_stations) #check to see that they are equivalent (340)
#hacky visual check: unique_stations$start_station_name[order(unique_stations$start_station_name)]

# count trips by gender
ggplot(data = trips, x = gender) + geom_histogram(mapping = aes(x = gender)) + xlab("Gender") + ylab("Total Number of Trips")
NROW(filter(trips, gender==2)) #female
NROW(filter(trips, gender==1)) #male
NROW(filter(trips, gender==0)) #unknown

# find the 10 most frequent station-to-station trips
sorted_trip_frequencies <- count(trips, start_station_name, end_station_name, sort = TRUE)
sorted_trip_frequencies[1:10,]

# count all trips that start and end on broadway
NROW(sorted_trip_frequencies)
=======

# find the earliest and latest birth years (see help for max and min to deal with NAs)

# use filter and grepl to find all trips that either start or end on broadway

# do the same, but find all trips that both start and end on broadway

# use filter, select, and distinct to find all unique station names

# count trips by gender

# find the 10 most frequent station-to-station trips

# use awk to count all trips that start and end on broadway
>>>>>>> 99e9ee98be50f773c22e73e60779dd63550a04d4
