library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)

load('trips.RData')
View(trips)
View(weather)

#---- FUNCTIONS

 mse <- function(lmfit)
   sqrt(mean((summary(lmfit)$residuals)^2))

#---- PREPARATIONS

df <- trips %>% group_by(ymd) %>% dplyr::summarise(total_trips = n())  
df <- inner_join(df, weather, "ymd") 

#===== fitting everything to see most significant ones



#selecting all 'fair' data
df <- select(df, ymd, total_trips, prcp, snwd, snow, tmin, tmax) 
View(df)

#based on unmodified data (i.e. no mutates)
lm.fit1 <- lm(total_trips ~ ., data = df)
summary(lm.fit1)
pred1 <- predict(lm.fit1, df)
df <- mutate(df, predicted = pred1)
View(df)

#plotting predicted vs real
ggplot(df, aes(predicted, total_trips)) + geom_point()

#plotting first regression
ggplot() + geom_point(aes(tmin, total_trips), data = df)
mse(lm.fit1)

#>>Conclusion: significant ones are prcp, snwd, tmax. tmin only somewhat.
#>>mean-squared error: 4627.437

#using only prcp, snws, tmax
lm.fit3 <- lm(total_trips ~ prcp + snwd + tmax, data = df)
mse(lm.fit3)

#>> MSE: 4654.822 (improvement by 27.385)

#===== is_weekend

#adding is_weekend T/F column
df <- mutate(df, is_weekend = wday(ymd) == 1 | wday(ymd) == 7)
View(df)

#double checking number of weekends that occured
total_weekends = nrow(df[df$is_weekend == TRUE,]) #104
total_weekdays = nrow(df[df$is_weekend == FALSE,]) #261 

#adding average number of trips grouped by weekend or not
group_weekend <- df %>% group_by(is_weekend) %>%  dplyr::summarise(total_occurances = n(), total_trips= sum(total_trips), avg_trips_per_day = total_trips/total_occurances) 
View(group_weekend)

#plotting graph
ggplot(group_weekend, aes(x=is_weekend, y = avg_trips_per_day)) + geom_point() #kida crappy but moving on

#adding is_weekend to model
lm.fit3 <- lm(total_trips ~ is_weekend + prcp + snwd + tmax, data = df)
summary(lm.fit3)
mse(lm.fit3) #3992.842

#=== is_holiday 

holiday_dates <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25")
df <- mutate(df, is_holiday = ymd %in% as.Date(holiday_dates))
View(df)

#=== adding percipitation

#first plotting precp with total_trips to get rough idea of natural cutoffs
dataprcp <- df %>% filter(prcp != 0 & prcp < 2)
isolate <- df %>% filter(prcp > 1.5 & total_trips > 20000)
View(isolate)
View(dataprcp)
ggplot(df, aes(log(prcp))) + geom_histogram() 

lm.fit2 <- lm(total_trips ~ tmax, data = df)
mse(lm.fit2)
pred2 <- predict(lm.fit2, df)
ggplot() + geom_point(aes(tmax * 10, total_trips), color = "red", data = df) + geom_point(aes(tmin * 10, total_trips), colour = "blue", data = df)



#-------------------

#fitting model (based on tavg)
tempavg <- mutate(df, tavg = (tmin+tmax)/2) %>% select(ymd, total_trips, prcp, snwd, snow, tavg, day, month)
#todo: add mutates