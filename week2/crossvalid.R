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
tww <- inner_join(tww, weather, "ymd") 
tww <- select(tww, ymd, tmin, total_trips) 
tww <- filter(tww, year(tww$ymd) == 2014)

View(tww)
nrow(tww)
View(weather)

#===== Q2 (splitting test / train)

index <- sample(1:nrow(tww), size = 0.2 * nrow(tww))
twwtest = tww[index, ]   #73
twwtrain = tww[-index,]  #293
nrow(twwtrain)

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

#===== Q5 (automating quadratic fitting)
Rtrain <- c()
Rtest <- c()
quadratic_power <- c()

for(k in 1:20){
  lm.quad <- lm(total_trips ~ tmin + poly(tmin, k), data=twwtrain)
  predict_train <- predict(lm.quad, twwtrain)
  predict_test <- predict(lm.quad, twwtest)
  quadratic_power[k] <- k
  Rtrain[k] <- cor(predict_train, twwtrain$total_trips) ^ 2
  Rtest[k] <- cor(predict_test, twwtest$total_trips) ^ 2
}

Rvalues <- data.frame(quadratic_power, Rtrain, Rtest)

View(Rvalues)
ggplot(Rvalues) + geom_line(aes(quadratic_power, Rtrain), color="red") + geom_line(aes(quadratic_power, Rtest), color="blue") + labs(color = "Test Data")

#===== Q6 (plotting graph with best k)

which.max(Rvalues$Rtest) #best fit: k = 4
Rvalues$Rtest[4] #0.6938476

which.min(Rvalues$Rtest) #worst fi: k = 20
Rvalues$Rtest[20] #0.0654375

lm.fit3 <- lm(total_trips ~ tmin + poly(tmin, 4), data = twwtrain) 
predicted1 <- predict(lm.fit3, twwtest)
ggplot() + geom_line(data = twwtest, aes(tmin, predicted1)) + geom_point(data=twwtest, aes(tmin, total_trips))

#==========[NOTES]==========
#detach("package:plyr", unload=TRUE) 