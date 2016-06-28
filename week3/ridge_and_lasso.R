install.packages("glmnet")
library(glmnet)
library(ISLR)

#===== Ridge Regression

#ommiting rows with NA
Hitters = na.omit(Hitters)
x = model.matrix(Salary ~ ., Hitters)[,-1] #taking everythign but AtBat, which is column 1
y = Hitters$Salary
View(Hitters)

#implementing a function over a grid of values ranging from lamda = 10^10 to 10^-2
grid = 10 ^ seq(10, -2, length = 100)
ridge.mod = glmnet(x, y, alpha = 0, lambda = grid) #creaing lambdas
grid

#setting random seed
set.seed(27)

#randomly chose a subset of nums between 1 and n, and use as indices for the training observations
train=sample(1:nrow(x), nrow(x)/2) #nrow(x) -> 263 #sample half the row randomly
test=(-train)
y.test=y[test]

#fit ridge regression on training set
ridge.mod = glmnet(x[train,], y[train], alpha = 0 ,lamda = grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])

#MSE using lamba = 4
mean((ridge.pred-y.test)^2) #11860.4

#comparing ridge with least squares (lamba == 4 > lamba == 0?)
ridge.pred=predict(ridge.mod, s = 0, newx=x[test,], extract=T)
mean((ridge.pred-y.test)^2) #117702.8

#choosing tuning parameter
set.seed(27)
cv.out = cv.glmnet(x[train,], y[train], alpha = 0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam #340.8295 = lamba that gives the smallest cross=validation error

#what is MSE associated with this value of lambda?
ridge.pred = predict(ridge.mod, s = bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2) #132690

#refit ridge regression model on full data set, using lambda chosen by cross validation
out=glmnet(x, y, alpha = 0)
predict(out,type="coefficients",s=bestlam)[1:20,]

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
