lm.fit = lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit
summary(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")

plot(lstat , medv)

abline(lm.fit, lwd = 3)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

lm.fit = lm(medv~lstat + age, data=Boston)
summary(lm.fit)
lm.fit = lm(medv ~ ., data = Boston)
summary(lm.fit)

#Interaction Terms
summary(lm(medv ~ lstat*age, data=Boston))

#Non-linear Transformations of the Predictors
lm.fit2 <- lm(medv~lstat + I(lstat^2))

#anova() compares models

#qualitative predictors
names(Carseats)
lm.fit = lm(Sales ~ . + Income:Advertising + Price:Age, data = Carseats)
summary(lm.fit)
contrasts(ShelveLoc) 

#Writing Functions
LoadLibraries = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

LoadLibraries()

