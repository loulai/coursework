library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
library(dplyr)
library(glmnet)
library(ISLR)
load('trips.RData')

##---- PREPARATION

df <- trips %>% group_by(ymd) %>% dplyr::summarise(total_trips = n())  
df <- inner_join(df, weather, "ymd") 
View(df)

#===== Ridge Regression

x = model.matrix(total_trips ~ is_hot*tmax + is_heavy_rain*prcp + is_holiday + is_weekend + snwd, df) #taking everythign EXCLUDING AtBat, which is column 1 (?)
y = df$total_trips
View(x)

#implementing a function over a grid of values ranging from lamda = 10^10 to 10^-2
grid = 10 ^ seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) #creating lambdas
ridge.mod
grid

#setting random seed
set.seed(1)

#randomly chose a subset of nums between 1 and n, and use as indices for the training observations
train=sample(1:nrow(x), nrow(x)/2) #nrow(x) -> 263 #sample half the row randomly
test=(-train)
y.test=y[test]

#fit ridge regression on training set
ridge.mod = glmnet(x[train,], y[train], alpha = 0, lambda = grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])

#MSE using lamba = 4
mean((ridge.pred-y.test)^2) #24,888,048

#comparing ridge with least squares (lamba == 4 > lamba == 0?)
ridge.pred=predict(ridge.mod, s = 0, newx=x[test,], extract=T)
mean((ridge.pred-y.test)^2) #25,192,857

#choosing tuning parameter
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #2573.666 = lamba that gives the smallest cross=validation error

#what is MSE associated with this value of lambda?
ridge.pred = predict(ridge.mod, s = bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2) #26,701,396

#refit ridge regression model on full data set, using lambda chosen by cross validation
out=glmnet(x, y, alpha = 0)
predict(out,type="coefficients",s=bestlam)[1:8,]

#================================== Lasso

#fit model, but alpha = 1
lasso.mod=glmnet(x[train,], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

#cross-validation and computing test error
set.seed(27)
cv.out=cv.glmnet(x[train,], y[train], alpha = 1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred = predict(lasso.mod, s = bestlam, newx=x[test,])
mean((lasso.pred-y.test)^2)
#143994.8
