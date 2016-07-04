library(e1071)
train <- data.frame(class=c("spam", "ham", "ham", "ham"),
                    viagra=c("yes", "no", "no", "yes"))

classifier <- naiveBayes(class ~ viagra, train)
classifier

test <- data.frame(viagra= c("yes"))
test$viagra <- factor(test$viagra, levels=c("no", "yes")) #factor levels have to be specified in the same order as the training dataset

View(test)

#feeding data into classifier object:
#predict(model used to predict, test data)
prediction <- predict(classifier, test, type="raw")
prediction

train <- data.frame(type=c("spam","ham","ham","ham"),
                    viagra=c("yes", "no", "no", "yes"),
                    meet=c("yes", "yes", "yes", "no"))

train

###### adding 'meet'
library(e1071)
classifier <- naiveBayes(type ~ viagra + meet, train)
classifier

test <- data.frame(viagra=c("yes"), meet=c("yes"))
test$viagra <- factor(test$viagra, levels=c("no", "yes"))
test$meet <- factor(test$meet, levels=c("no", "yes"))
test

prediction <- predict(classifier, test, type = "raw")
prediction

######## Exsercises
prod <- data.frame(buy=c("yes", "no", "no", "yes"),
                   income=c("high", "high", "medium", "low"))
prod

classifier1 <- naiveBayes(buy ~ income, prod)

#adding income = high (what is the prob that customer will buy, given high income?)
test1 <- data.frame(income="high") 
test1$income <- factor(test1$income, levels=c("high", "medium", "low")) #adding levels
test1

prediction1 <- predict(classifier1, test1, type="raw")
prediction1
#no yes
#[1,] 0.5 0.5

#adding gender
prod <- data.frame(buy=c("yes", "no", "no", "yes"),
                   income=c("high", "high", "medium", "low"),
                   gender=c("male", "female", "female", "male"))

classifier1 <- naiveBayes(buy ~ income + gender, prod)
classifier1
#50/50 chance had high income. 100% chance buyer is male

test1 <- data.frame(income=c("high"), gender=("male"))
test1$income <- factor(test1$income, levels=c("high", "medium", "low"))
test1$gender <- factor(test1$gender, levels=c("female", "male"))
test1

prediction2 <- predict(classifier1, test1, type="raw")
prediction2
#     no         yes
#[1,] 0.000999001 0.999001
#buy only when male
