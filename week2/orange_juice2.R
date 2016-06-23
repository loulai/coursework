library(ggplot2)
oj <- read.csv("oj.csv")

#=== Q1 (how demographics affect demand)

lm.fit1 <- lm(logmove ~ log(price) * brand * feat, oj)
summary(lm.fit1) #rsq=0.5354 

lm.fit2 <- lm(logmove ~ log(price) * brand * feat + AGE60, oj)
summary(lm.fit2) #rsq=0.5488  t=23.354

lm.fit3 <- lm(logmove ~ log(price) * brand * feat + EDUC, oj)
summary(lm.fit3) #rsq=0.5357  t=4.46

lm.fit4 <- lm(logmove ~ log(price) * brand * feat +  ETHNIC, oj)
summary(lm.fit4) #rsq=0.5417  t=19.873

lm.fit5 <- lm(logmove ~ log(price) * brand * feat + INCOME, oj)
summary(lm.fit5) #rsq=0.5389  t=-14.92

lm.fit6 <- lm(logmove ~ log(price) * brand * feat + AGE60 + EDUC + ETHNIC + INCOME, oj)
summary(lm.fit6) #rsq=0.5681  

#all are > 2
#r improved from 0.5354 to 0.5681

#====== Q2 (focus on HHLarge and EDUC)

mean_hh = mean(oj$HHLARGE)
View(mean_hh) #0.1156024

mean_educ = mean(oj$EDUC)
View(mean_educ) #0.2252196

summary(oj$HHLARGE)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.01351 0.09794 0.11120 0.11560 0.13520 0.21640 

summary(oj$EDUC)
# Min.    1st Qu. Median  Mean    3rd Qu. Max. 
# 0.04955 0.14600 0.22940 0.22520 0.28440 0.52840 

lm.fit7 <- lm(logmove ~ log(price) * brand * feat + HHLARGE, oj)
summary(lm.fit7) #0.5416  t=-19.872

lm.fit8 <- lm(logmove ~ log(price) * brand * feat + EDUC, oj)
summary(lm.fit8) #0.5416  t=-19.872

#median thingy
HHbeta <- -2.67685 
HHchange <- (HHbeta * 0.13520) - (HHbeta * 0.1120) #Q3 - Median
HHchange #-0.0621

EDUbeta <- 0.16576
EDUchange <- (EDUbeta * 0.28440) - (EDUbeta * 0.22940) #Q3 - Median
EDUchange #0.0091

#Household size > Education for prediction

#=== Q2 iii (interaction terms)

# Added interaction
lm.fit9 <- lm(logmove ~ log(price) * brand * feat + log(price) * HHLARGE + log(price) *  EDUC, oj)
summary(lm.fit9)

# No interaction 
lm.fit10 <- lm(logmove ~ log(price) * brand * feat + HHLARGE + EDUC, oj)
summary(lm.fit10) 

#                               Interaction   No Interaction
# HHLARGE                           0.91991   -2.87752 
# EDUC                             -3.05531   -0.14131

# log(price):HHLARGE               -4.72898    
# log(price):EDUC                   3.69490 

# log(price)                       -3.09620   -2.79278 

#Ans: Logmove is much more sensitive to price with the interaction term. i.e. hold HHL constant, but increase price by 1. 
#There will be more sales lost when interaction term is considered.

#=== Q3 (training / test split)

index <- sample(1:nrow(oj), size = 0.2 * nrow(oj))
ojtest = oj[index, ]   #5789
ojtrain = oj[-index,]  #23158

#=== Q4 

lm.fit11 <- lm(logmove ~ log(price) + brand, ojtest)
summary(lm.fit11)
#rsq=0.3911

predicted_sales <- predict(lm.fit11, ojtest)
cor(predicted_sales, ojtest$logmove) ^ 2
#honest rsq=0.391149

#=== Q5

lm.fit12 <- lm(logmove ~ brand*log(price)*feat, ojtest)
summary(lm.fit12)
#rsq=0.5277

predicted_sales2 <- predict(lm.fit12, ojtest)
cor(predicted_sales2, ojtest$logmove) ^ 2
#honest rsq=0.5276755

#both models do alright on the test data
#rsq only increase a little

lm.fit13 <- lm(logmove ~ brand*log(price)*feat + AGE60 + EDUC + ETHNIC + INCOME, ojtrain)
summary(lm.fit13)
#rsq=0.5707

predicted_sales3 <- predict(lm.fit13, ojtest)
cor(predicted_sales3, ojtest$logmove) ^ 2
#rsq=0.557498

#rsq became worse, presumably because test was overfitted




















#==========================[JUNK]==========================
#plot_data <- expand.grid(price = unique(oj$price),
                        # HHLARGE = quantile(oj$HHLARGE, c(0.25, 0.5, 0.75)),
                        # EDUC = quantile(oj$EDUC, c(0.25, 0.5, 0.75)))
#plot_data$predicted <- predict(lm.fit8, plot_data)
