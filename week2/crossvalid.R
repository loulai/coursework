library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)

load('trips.RData')
View(trips)
View(weather)

#====== Q1 (making data frame. Day, trips, tmin)
View(trips)
?filter
#tww = trips with weather
tww <- trips  %>% group_by(ymd) %>% dplyr::summarise(total_trips = n())  #otherwise conflicts with plyr
tww <- inner_join(tday, weather, "ymd") 
tww <- select(tday, ymd, tmin, total_trips) 
tww <- filter(tww, year(tww$ymd) == 2014)

View(tww)
nrow(tww)
View(weather)

#===== Q2 (splitting test / train)

index <- sample(1:nrow(tww), size = 0.2 * nrow(tww))
twwtest = tww[index, ]   #73
twwtrain = tww[-index,]  #293

#===== Q3 (fitting model)
 
#  i.run regression on training data set
lm.fit1 <- lm(total_trips ~ tmin, twwtrain)

# ii.predict values
predicted_trips <- predict(lm.fit1, twwtest)

#iii.evaluate model on test data set
#graphing training & test data. Regression line is modeled on the training data, which is gray. It's designed to fit the test data in red.
ggplot() + geom_point(data=twwtrain, aes(tmin, total_trips), color = "gray") + geom_point(data=twwtest, aes(tmin, total_trips, color = "red")) + geom_line(data=twwtest, aes(tmin, predicted_trips, color = "red")) + labs(color = "Test Data")
corr <- cor(predicted_trips, twwtest$total_trips) ^ 2
corr #0.7075393

#===== Q4 (fitting model with quadratic)

lm.fit2 <- lm(total_trips ~ tmin + poly(tmin, 2), data=twwtrain)
quad_predicted_trips <- predict(lm.fit2, twwtest)
ggplot() + geom_point(data=twwtrain, aes(tmin, total_trips), color = "gray") + geom_line(data=twwtest, aes(tmin, quad_predicted_trips, color = "blue")) + geom_point(data=twwtest, aes(x = tmin, y = total_trips, color = "grey")) + geom_line(data=twwtest, aes(tmin, predicted_trips, color = "red")) + labs(color = "Test Data")
quad_corr <- cor(quad_predicted_trips, twwtest$total_trips) ^ 2
quad_corr #0.7344676, improved!

View(twwtrain)





#==========[NOTES]==========
#detach("package:plyr", unload=TRUE) 