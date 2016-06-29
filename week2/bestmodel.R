library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)

load('trips.RData')
View(trips)
View(weather)


#---- PREPARATION

df <- trips %>% group_by(ymd) %>% dplyr::summarise(total_trips = n())  
df <- inner_join(df, weather, "ymd") 
View(df)

#---- FUNCTIONS

mse <- function(lmfit)
  sqrt(mean((summary(lmfit)$residuals)^2))

graph_predicted <- function(lmfit){
  pred <- predict(lmfit, df)
  df <- mutate(df, predicted = pred)
  ggplot() + geom_point(aes(predicted, total_trips), data = df) 
}



#===== fitting everything to see most significant ones

#based on unmodified data (i.e. no mutates)
lm.fit1 <- lm(total_trips ~ ., data = df)
summary(lm.fit1)
pred1 <- predict(lm.fit1, df)
df <- mutate(df, predicted = pred1)
View(df)

#plotting predicted vs real
ggplot(df, aes(predicted, total_trips)) + geom_point()

#plotting regression: predicted is red, actual is grey
ggplot(data = test) + geom_point(aes(tmin, total_trips), color = "gray") + geom_point(aes(tmin, predicted), color = "red")
mse(lm.fit1)

#>>Conclusion: significant ones are prcp, snwd, tmax. tmin only somewhat.
#>>mean-squared error: 4625.261

#using only prcp, snws, tmax
lm.fit3 <- lm(total_trips ~ prcp + snwd + tmax, data = df)
pred3 <- predict(lm.fit3, df)
df <- mutate(df, predicted3 = pred3)
View(df)
mse(lm.fit3)

#>> MSE: 4654.822 (improved by 27.385)

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
mse(lm.fit3) 

#>> MSE: 3992.842 (improved by 661.96)

#===== is_holiday 

holiday_dates <- c("2014-01-01", "2014-01-20", "2014-02-17", "2014-05-26", "2014-07-04", "2014-09-01", "2014-10-13", "2014-11-11", "2014-11-27", "2014-12-25")
df <- mutate(df, is_holiday = ymd %in% as.Date(holiday_dates))
View(df)

#adding is_holiday to model
lm.fit4 <- lm(total_trips ~ is_holiday + is_weekend + prcp + snwd + tmax, data = df)
summary(lm.fit4)
mse(lm.fit4)

#>> MSE: 3737.511 (improved by 255.331)

#===== is_heavy_percipitation

#multiplying prcp by 10
rain_df <- mutate(df, prcp = prcp %/% 0.1) %>% filter(prcp < 30)

#counting how many days a certain percipitation occured, and how many trips were on that day
raining <- rain_df %>% group_by(prcp) %>% dplyr::summarise(days_occured = n(), total_trips = sum(total_trips))
View(raining)

#seeing the average number of trips taken on that day
raining <- mutate(raining, avg_trips_that_percipitation = total_trips/days_occured)
View(raining)

ggplot(raining, aes(prcp, avg_trips_that_percipitation)) + geom_point()

#still doesn't seem to be any obvious bins.. hence will use 4 > to indicate is_heavy_rain
View(df)
df <- mutate(df, is_heavy_rain = (prcp*10) > 4)

#adding is_heavy_rain
lm.fit5 <- lm(total_trips ~  is_heavy_rain + is_holiday + is_weekend + prcp + snwd + tmax, data = df)
summary(lm.fit5)
mse(lm.fit5)
#>> MSE: 3651.79 (improved by 85.721)

#===== is_heavy_rain * prcp
lm.fit6 <- lm(total_trips ~ is_heavy_rain*prcp + is_holiday + is_weekend + snwd + tmax, data = df)
mse(lm.fit6)
#>> MSE: 3315.768 (improved by 336.022)

#===== is_hot
View(df)
df <- mutate(df, is_hot = (tmax*10) > 80)
lm.fit7 <- lm(total_trips ~ is_hot + is_heavy_rain*prcp + is_holiday + is_weekend + snwd + tmax, data = df)
mse(lm.fit7)
#>> MSE: 3196.921 (improved by 118.847)

#===== is_hot * tmax
lm.fit8 <- lm(total_trips ~ is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, data = df)
summary(lm.fit8)
mse(lm.fit8)
#>> MSE: 3112.111 (improved by 84.81)

#===== is_cold
df <- mutate(df, is_cold = (tmax*10) < 50)
lm.fit9 <- lm(total_trips ~ is_cold + is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, data = df)
mse(lm.fit9)
#>> MSE: 3088.501 (improved by )

#===== is_cold * tmin
lm.fit10 <- lm(total_trips ~ is_cold*tmin + is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, data = df)
summary(lm.fit10)
mse(lm.fit10)
#>> MSE: 2993.919 (improved by )

#===== day_of_week
df <- mutate(df, day_of_week = wday(ymd))
View(df)
lm.fit11 <- lm(total_trips ~ day_of_week + is_cold*tmin + is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, data = df)
summary(lm.fit11)
mse(lm.fit11)
#>> MSE: 2960.2 (improved by )

#===== graphing predicted v real
graph_predicted(lm.fit11) 

#===== viewing outliers
View(outliers <- df %>% filter(abs(total_trips) - abs(predicted) > 6000))
pred <- predict(lmfit11, df)
df <- mutate(df, predicted = pred)
ggplot() + geom_point(aes(predicted, total_trips), color = "grey", data = df) + geom_point(aes(predicted, total_trips), color = "red", data = outliers)

#===== viewing snow days
View(snowdays <- df %>% filter(snwd > 0))

#===== is_snowing 
df <- mutate(df, is_snowing = snow > 0)
View(df)
lm.fit12 <- lm(total_trips ~ is_snowing + is_cold*tmin + is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, data = df)
summary(lm.fit12)
mse(lm.fit12)
#>> MSE: 2993.756 (improved by a fraction, not worthy)



#-------------------
#fitting model (based on tavg)
tempavg <- mutate(df, tavg = (tmin+tmax)/2) %>% select(ymd, total_trips, prcp, snwd, snow, tavg, day, month)
#todo: add mutate