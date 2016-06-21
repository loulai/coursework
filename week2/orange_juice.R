library(ggplot2)
oj <- read.csv("oj.csv")

#====
#price distribution
ggplot(oj, aes(price)) + geom_histogram() + geom_vline(xintercept = mean(oj$price), linetype = 2, color = "red")
ggplot(oj, aes(price)) + geom_histogram() + geom_vline(xintercept = mean(oj$price), linetype = 2, color = "red") + scale_x_log10()

#brand distribution
levels(oj$brand)

dom <- oj %>% filter(brand == "dominicks") 
ggplot(dom, mapping = aes(dom$price)) + geom_histogram() + geom_vline(xintercept = mean(dom$price), linetype = 2, color = "red")

minmaid <- oj %>% filter(brand == "minute.maid") 
ggplot(minmaid, mapping = aes(minmaid$price)) + geom_histogram() + geom_vline(xintercept = mean(minmaid$price), linetype = 2, color = "red")

trop <- oj %>% filter(brand == "tropicana") 
ggplot(trop, mapping = aes(trop$price)) + geom_histogram() + geom_vline(xintercept = mean(trop$price), linetype = 2, color = "red")

ggplot() + geom_histogram(mapping = aes(dom$price, fill = "red")) + geom_histogram(mapping=aes(minmaid$price, fill = "blue")) + geom_histogram(mapping=aes(trop$price, fill = "yellow")) 

#3. logmove
ggplot(oj, aes(x = logmove, y = log(oj$price), color = brand)) + geom_point() + xlab("log of price") + ylab("log of price")

#4 regression
lm.fit1 <- lm(oj$logmove ~ log(oj$price))
summary(lm.fit1)

#with brand
lm.fit2 <- lm(logmove ~ log(price) + brand, data = oj)
summary(lm.fit2)
anova(lm.fit1, lm.fit2)
#elasticity is -1.6, which makes sense.
#not a great fit, R squared 0.2 and 0.39 for brand
#as price increases by 1, quantity will decrease by 1.6
#as brands are considered, sales will increase. Tropicana > minutemaid

#with interaction term
lm.fit3 <- lm(logmove ~ log(price) * brand, data = oj)
summary(lm.fit3)
#virtually same rsquares
#the price increase will decrease poportionally less
#individual price elasticities will decrease, but that is becuase price interaction term is considered

View(oj)

