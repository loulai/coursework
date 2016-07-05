library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
cor(Smarket[,-9]) #can see correlation between year & volume, i.e. avg num of shares traded daily increases form 2001-5
attach(Smarket)
plot(Volume)


#### Logistic Regression
glm.fit=glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data=Smarket, family=binomial)

summary(glm.fit)

coef(glm.fit)

summary(glm.fit)$coef

glm.probs=predict(glm.fit, type = "response")
glm.probs[1:10]

#creates a vector of 1,250 Down elements
glm.pred=rep("Down", 1250)

#transforms to UP wjere the predicted prob of a market increase exceeds 0.5
glm.pred[glm.probs > .5] = "Up"

#confusion matrix
table(glm.pred, Direction)

#the faction of days for which the prediction was correct
mean(glm.pred == Direction) #0.5216. Logistic regression correctly predicts market movement 52.5% of the time

########### testing on future

train = (Year < 2005) #data from 2001 - 2004
Smarket.2005 = Smarket[!train,] #data from 2005

dim(Smarket.2005)
Direction.2005=Direction[!train] #what is this

#logistic reg model using only the subset of the observations where dates < 2005
glm.fit = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family=binomial, subset=train)

#predict probabilities of the stock market going up for each days in our test set (i.e. days in 2005)
glm.probs = predict(glm.fit, Smarket.2005, type="response")

#compute predictions for 2005 
glm.pred = rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005) #0.4801587
mean(glm.pred != Direction.2005) #0.5198413. Error rate = 52% !!

#simplyfying model for Lag1 + Lag2
glm.fit=glm(Direction ~ Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fit, Smarket.2005, type="response")
glm.pred=rep("Down", 252)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)

106 / (106 + 76)

#prediction Direction on a day where Lag1 and Lag2 
#1.2 and 1.1 respectively, and on a day the equal 1.5 and -0.8
predict(glm.fit, newdata = data.frame(Lag1=c(1.2,1.5),
                                      Lag2=c(1.1, -0.8)), type="response")