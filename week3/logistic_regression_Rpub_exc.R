library(dplyr)
library(ggplot2)
library(stargazer)
t <- read.csv("https://www.dropbox.com/s/eg6kack8wmlqmhg/titanic_train.csv?raw=1")
View(t)

### 1. odds of surviving
df1 <- t %>% group_by(Survived) %>% summarize(num = n())
View(df1)
survival_odds =  df1$num[2] / df1$num[1] 
survival_odds #0.6229508 = ratio

### 2. how much lower are the odds of survival for men relative to women?
logit1 <- glm(Survived ~ Sex, data=t, family = "binomial")
summary(logit1)
exp(coef(logit1))
#(Intercept)     Sexmale 
#2.87654321  0.08096732  odds are 92% lower for male

#using probabilities
test_mf <- data.frame(Sex=c("female", "male"))
test_mf$pred <- predict(logit1, test_mf, type="response")
View(test_mf)
# >> male: 19.0%, female: 74.2% probability that they will survive

### controlling for gender, does age have an impact on odds of survival? What magnitude?
logit2 <- glm(Survived ~ Sex + Age, data = t, family = "binomial")
summary(logit2)
exp(coef(logit2))
# Age 
#0.99458879 >> holding all else constant, age will decrease survival odds by 1% (why tho)

#using probabilities 
#getting 11223344, fmfmfmf
vec_age = c()
vec_sex = c()
for (num in 1:80){ #80 is the max age
  vec_age <- append(vec_age, num)
  vec_age <- append(vec_age, num)
  vec_sex <- append(vec_sex, "female")
  vec_sex <- append(vec_sex, "male")
}
vec_age
vec_sex

age_sex <- data.frame(Age=vec_age, Sex = vec_sex)
View(age_sex)

test_age <- age_sex

test_age$pred <- predict(logit2, test_age, type="response") #predicting probabilites
View(test_age)

test_age <- arrange(test_age, desc(pred))
#wow, the oldest woman still has the highest chance of surviving compared to the youngest male

##### controlling for gender, does class have an effect? magnitude?
logit3 <- glm(Survived)

