library(dplyr)
library(ggplot2)
library(lubridate)
library(plyr)

load('trips.RData')
View(trips)
View(weather)

#======

tday <- trips %>% group_by(ymd) %>% summarize(total_trips) 
tday <- join(tday, weather, "ymd")
tday <- select(tday, ymd, tmin, total_trips)

ggplot(tday, aes(x = tmin, y = total_trips)) + geom_point()

View(tday)

View(weather)
