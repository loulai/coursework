library(dplyr)
library(stargazer)
library(caret)
loan <- read.csv("https://www.dropbox.com/s/89g1yyhwpcqwjn9/lending_club_cleaned.csv?raw=1")
summary(loan)

#fitting model
logit1 <- glm(good ~ fico, family = "binomial", data = loan)
summary(logit1)

View(loan)

#exponential of the coefficients
exp(coef(logit1)) # -> 1.0124345145 
#if fico increases by 1, the odds ratio of loan being good increases by 1.2%

#exercise
logit2 <- glm(good ~ dti, family = "binomial", data = loan )
summary(logit2)
coef(logit2)
#if dti increases by 1, odds ration of loan being good increases by 0.9833%

########## 4
#what is the effect of fico going from 700 to 750
test <- data.frame(fico = c(700, 750))
test$pred <- predict(logit1, test, type="response")
test

#exsecise 2
#effect of fico going from 750-800?
test2 <- data.frame(fico=c(750, 800))
test2$pred <- predict(logit1, test2, type="response")
test2 #increases good by 4%

######### 5 [interpreting coefficients]
logit3 <- glm(good ~ fico + loan_amnt, data = loan, family = "binomial")
summary(logit3)
exp(coef(logit3))

######## 6 [categorical variables]
logit4 <- glm(good ~ fico + loan_amnt + purpose, data = loan, family = "binomial")
summary(logit4)
round(exp(coef(logit4)),3)

######## 7 [dealing with missing values]
#adding income
logit5 <- glm(good ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
summary(logit5)
exp(coef(logit5))

####### 8 [stargazer]
stargazer(logit1, logit3, logit4, logit5, type="text")

####### 9 [testing logistic model out of sample]
#splitting test/train
set.seed(364)
sample <- sample(nrow(loan), floor(nrow(loan)*0.8))
train <- loan[sample,]
test <- loan[-sample,]

logit5 <- glm(good ~ fico + loan_amnt + income + purpose, data = loan, family = "binomial")
test$pred <- predict(logit5, test, type="response")

test$good_pred <- ifelse(test$pred > 0.80, "good", "bad")
confusionMatrix(test$good_pred, test$good)
#accuracy of 72%
#detect bad loans 37% of cases
#label good loans good in 79% of cases