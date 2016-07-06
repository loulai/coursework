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
logit1 <- glm(Survived ~ Sex, family = "binomial", data = t)
summary(logit1)
exp(coef(logit1))

test1 <- data.frame(Sex=c("Male", "Female"))
test1$pred <- predict(logit1, test1, type="response")
test1

df2 <- t %>% group_by(Sex, Survived) %>% summarize(total_people = sum(Freq)) 

df2 <- mutate(df, m_survival_ratio = )
View(df2)

#drob

l1 <- glm(Survived ~ 0, data=t, family="binomial")
summary(l1)
round(exp(coef(l1)),2)
