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

#================Exercises
#==8
attach(Auto)
lm.fit3 = lm(mpg ~ horsepower, data = Auto)
summary(lm.fit3)
#beta and p for Fstat is low, thus there is relationship
#0.6 r squared is strong
#negative relationship

confint(lm.fit3)
#8.525212 41.3465103

plot(horsepower, mpg)
abline(lm.fit3)
#the data does not look linear

#==9
pairs(Auto)
cor(Auto[sapply(Auto, is.numeric)])
lm.fit4 = lm(mpg ~ . , data=Auto)
summary(lm.fit4)
#beta and p for Fstat is low, thus thre is relationship
#ones with stars are statistitically significant (p == 0)
#the larger the year, the higher the mpg
plot(lm.fit4)
#outlier residuals towards the end, no high leverage
lm.fit5 = lm(mpg ~ year * weight, data = Auto)
View(lm.fit5)
summary(lm.fit5)
lm.fit6 = lm(mpg ~ year)
lm.fit7 = lm(mpg ~ year + I(year^2))
anova(lm.fit6, lm.fit7)
#F-stat is 18.758, low P stat. Reject the hyp that both fil equally well, 
summary(lm.fit6)

#==10
lm.fit8 = lm(Sales ~ Price + Urban + US, data=Carseats)
contrasts(Urban)
contrasts(US)
summary(lm.fit8)

#Sales are lower when house is urban, higher if it is US
#but urbanYES seems to have high p value
#can reject Urban as a predictor
lm.fit9 = lm(Sales ~ Price + US, data=Carseats)
anova(lm.fit8, lm.fit9)
#low F and high P, doesn't seem to fit much better, thus ca'nt reject that both graphs model equally well
summary(lm.fit9)
confint(lm.fit8)
confint(lm.fit9)
#conf intervals for Betas with/out the Urban don't change much
plot(lm.fit9)
#one point with super super high leverage, but low-medium standardized residuals