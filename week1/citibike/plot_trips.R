########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(tripduration)) + geom_histogram() + xlim(0,60*100)

# plot the distribution of trip times by rider type
ggplot(trips, aes(tripduration, fill = usertype)) + geom_histogram() + xlim(0,60*100)

# plot the number of trips over each day
ggplot(trips, aes(x = ymd)) + geom_histogram()

# plot the number of trips by gender and age
ggplot(trips, aes(x = birth_year, fill = as.factor(gender))) + geom_histogram(position = "identity")

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio

########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather, aes(x = ymd, y = tmin)) + geom_point()

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
newdf <- trips_with_weather %>% group_by(ymd) %>% summarize(num = n())
newdf2 <- inner_join(newdf, weather, by="ymd")
ggplot(newdf2, mapping = aes(x = tmin, y = num)) + geom_point() + geom_smooth()

  #>> in one line
trips_with_weather %>% group_by(ymd) %>% summarize(num = n()) %>% inner_join(weather, by="ymd") %>% ggplot(aes(x = tmin, y = num)) + geom_point() + geom_smooth()


# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>% group_by(ymd) %>% summarize(num = n()) %>% inner_join(weather, by="ymd") %>% mutate(TFperp = prcp > 40) %>% ggplot(mapping = aes(x = tmin, y = num, color = TFperp)) + geom_point() 

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>% group_by(ymd) %>% summarize(num = n()) %>% inner_join(weather, by="ymd") %>% mutate(TFperp = prcp > 40) %>% ggplot(mapping = aes(x = tmin, y = num, color = TFperp)) + geom_point() + geom_smooth()

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
View(trips %>% mutate(starthour = hour(starttime), themonth = month(ymd)) %>% select(starttime, ymd, starthour, themonth) %>% group_by(starthour,themonth))
sdtrip <- trips %>% mutate(hour = hour(starttime)) %>% group_by(ymd, hour) %>% summarize(num_trips=n()) %>% group_by(hour) %>% summarize(mean_trips = mean(num_trips), sd_trips = sd(num_trips))
ggplot(sdtrip, mapping= aes(x = hour, y = mean_trips)) + geom_smooth() + geom_errorbar(ymin = sdtrip$mean_trips - sdtrip$sd_trips, ymax = sdtrip$mean_trips + sdtrip$sd_trips)

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
