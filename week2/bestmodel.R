library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)

load('trips.RData')
View(trips)
View(weather)

#===== (fitting everything, chosing most significant)

joined <- trips %>% group_by(ymd) %>% dplyr::summarise(total_trips = n())  
joined <- inner_join(joined, weather, "ymd") 

#selecting all 'fair' data, and adding day/week/month data
joined <- select(joined, ymd, total_trips, prcp, snwd, snow, tmin, tmax) 

View(joined)

#based on unmodified data (i.e. no mutates)
lm.fit1 <- lm(total_trips ~ ., data = joined)
summary(lm.fit1)
#significant ones are prcp, snwd, tmax. tmin only somewhat.

# todo: rsq, mean error, cross validation

#fitting model (based on tavg)
tempavg <- mutate(joined, tavg = (tmin+tmax)/2) %>% select(ymd, total_trips, prcp, snwd, snow, tavg, day, month)
#todo: add mutates