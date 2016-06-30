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
index <- sample(1:nrow(df), size = 0.2 * nrow(df))
train = df[-index,]      #292
validation = df[index,]  #73

#---- FUNCTIONS

mse <- function(lmfit)
  sqrt(mean((summary(lmfit)$residuals)^2))

if(FALSE){ #_______IGNORE___________
  mse.avg <- function(predictors)
    array_of_mse[]
  while (i < 21){
    #creating random 20% test
    index <- sample(1:nrow(df), size = 0.2 * nrow(df))
    train = df[-index,]      #292
    validation = df[index,]  #73
    model <- lm(df$total_trips, )
  }
  
  
  make_lm <- function(p1){
    index <- sample(1:nrow(df), size = 0.2 * nrow(df))
    train = df[-index,]      #292
    model <- lm.fit(total_trips ~ p1, data = train)
    summary(model)
  }
  make_lm("prcp")
  # lm.fit1 <- lm(total_trips ~ train[,2:5], data=train)
} #_____________IGNORE_______________


cross_mse<- function(lmfit, folds){
  View(df)
  shuffled <- df[sample(nrow(df)),] #shuffling data frame
  i = 1 #initializing counter
  array_mse <- c(1,2,3) #dummy numbers to initiate array
  while(i < folds){
    index = i * (nrow(shuffled)/folds)
    array_mse[i] <- mse(lmfit, shuffled[1:index,])
    print(array_mse[i])
    i = i + 1
  }
  return(mean(array_mse))
}

#===== fitting everything to see most significant ones

#selecting all 'fair' data
df <- select(df, ymd, total_trips, prcp, snwd, snow, tmin, tmax) 
View(df)

#based on unmodified data (i.e. no mutates)
View(train)
lm.fit1 <- lm(total_trips ~ ., data = train)
summary(lm.fit1)
pred1 <- predict(lm.fit1, validation)
validation <- mutate(validation, predicted = pred1)
View(validation)

#plotting predicted vs real
ggplot(validation, aes(predicted, total_trips)) + geom_point()

#plotting regression: predicted is red, actual is grey
ggplot(data = test) + geom_point(aes(tmin, total_trips), color = "gray") + geom_point(aes(tmin, predicted), color = "red")
mse(lm.fit1)

#>>Conclusion: significant ones are prcp, snwd, tmax. tmin only somewhat.
#>>mean-squared error: 3956.24

#using only prcp, snws, tmax
lm.fit3 <- lm(total_trips ~ prcp + snwd + tmax, data = train)
pred3 <- predict(lm.fit3, validation)
validation <- mutate(validation, predicted3 = pred3)
View(validation)
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