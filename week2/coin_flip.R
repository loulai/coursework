library(ggplot2)
library(reshape)
library(dplyr)
theme_set(theme_bw())
set.seed(42)

estimate_coin_bias <- function(n,p){
  mean(rbinom(n,1,p))
}

View(estimate_coin_bias)

n <- 1000
p <- 0.3
p_hat <- replicate(1e5, estimate_coin_bias(n,p))

qplot(x=p_hat, geom="histogram", binwidth=0.01) + geom_vline(xintercept=0.3, color = "blue") + geom_vline(xintercept=mean(p_hat), linetype=2, color="yellow")

#yellow represents the mean calculated by averaging the coin flips
#blue represents 'true' mean (only known becuase we hardcodede it in)

#=====
#repeating for different sample sizes
plot_data <- data.frame()
for (n in c(100, 200, 400, 800)) {
  tmp <- data.frame(n=n, p_hat=replicate(1e5, estimate_coin_bias(n, p)))
  plot_data <- rbind(plot_data, tmp)
}

qplot(data=plot_data, x=p_hat, geom="histogram", binwidth=0.01, facets = . ~ n)
#can see that the more samples, hte more pointy (i.e. centered around true mean) the data gets

#===
#standard error (se)

se <- plot_data %>%
  group_by(n) %>%
  summarize(se=sd(p_hat))
qplot(data=se, x=n, y=se) +
  stat_function(fun=function(n) {sqrt(p * (1 - p) / n)}, linetype=2)