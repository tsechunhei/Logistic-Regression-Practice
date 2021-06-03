#############################################Carseats Practice########################
#Predict Sales using Price, Urban, and US:
library(stargazer)
load("Carseats.rda")

lm1 = lm(Sales ~ Price + as.factor(Urban) + as.factor(US), Carseats)
stargazer(lm1, type='text')

lm2 = lm(Sales ~ Price + as.factor(US), Carseats)
stargazer(lm2, type='text')

confint(lm2)

#############################################Linear Regression Simulation########################
#Simulate data
set.seed(1)
windows(width=10, height=8)
x = rnorm(100)
eps = rnorm(100, 0, 0.25)
y = -1 + 0.5*x + eps
plot(x,y)

#Predict y using x.
windows(width=10, height=8)
x = rnorm(100)
eps = rnorm(100, 0, 0.25)
y = -1 + 0.5*x + eps
plot(x,y)
fit1= lm(y~x)
abline(coef(fit1))

#Scatterplot:
windows(width=10, height=8)
x<- rnorm(100)
eps<- rnorm(100, 0, 0.25)
y<- -1 + 0.5*x + eps
plot(x,y)
fit1<- lm(y~x)
abline(coef(fit1))
abline(-1,0.5,col = 'red',lty = 2)
legend('topleft',
       legend = c("OLS regression","Population regression"),
       col = c('black','red'),
       lty = 1:2, cex = 0.8,
)

#Polynomial regression
fit2= lm(y~x+I(x^2))
summary(fit2)


#Less noise approach:
windows(width=10, height=8)
x3 = rnorm(100)
eps= rnorm(100, 0, 0.1)
y3 = -1 + 0.5*x3 + eps
plot(x3,y3)
fit3= lm(y3~x3)
summary(fit3)
abline(coef(fit3))
abline(-1,0.5,col = 'red',lty = 2)
legend('topleft',
       legend = c("OLS regression","Population regression"),
       col = c('black','red'),
       lty = 1:2, cex = 0.8,
)

#More noise approach:
windows(width=10, height=8)
x4 = rnorm(100)
eps = rnorm(100, 0, 0.5)
y4 = -1 + 0.5*x4 + eps
plot(x4,y4)
fit4= lm(y4~x4)
summary(fit4)
abline(coef(fit4))
abline(-1,0.5,col = 'red',lty = 2)
legend('topleft',
       legend = c("OLS regression","Population regression"),
       col = c('black','red'),
       lty = 1:2, cex = 0.8,
)


confint(fit1)
confint(fit3)
confint(fit4)


#############################################Logistic Regression ########################
#Preliminaries
library(magrittr)
library(gridExtra)
library(ggplot2)
library(dplyr)
df = read.csv('wells.csv')
df = df %>%
  mutate(log_arsenic = log(arsenic))

#Histogram of arsenic and larsenic:
arsenic_plot = ggplot(df) +
  geom_histogram(aes(x = arsenic), binwidth = 0.1, color = 5) +
  xlab('micrograms per liter') +
  ggtitle('arsenic')

log_arsenic_plot = ggplot(df) +
  geom_histogram(aes(x = log_arsenic), binwidth = 0.1, color = 3) +
  xlab('log(micrograms per liter)') +
  ggtitle('Log arsenic')

grid.arrange(arsenic_plot, log_arsenic_plot, ncol = 2)

#Z-score:
df = df %>%
  mutate(dist100 = dist / 100,
         z_education = (educ - mean(educ)) / sd(educ))

#Using dist100 to predict switch:
fit1 = glm(switch ~ dist100, family = binomial(link = 'logit'), df)
summary(fit1)

#Plots
ggplot(df, aes(x = dist100, y = switch)) +
  geom_jitter(height = 0.15) +
  stat_smooth(method='glm',
              method.args = list(family = "binomial"),
              formula = y ~ x) +
  xlab('Distance') +
  ylab("Prob")


#Predicted probability of switching wells for the average household:
avg_dist = df %>%
  summarize(avg_dist = mean(dist100)) %>%
  pull(avg_dist)
predict(fit1, newdata = data.frame(dist100 = avg_dist), type = 'response')


#Marginal effect of dist100 for the average household:
lambda = function(z) {
  exp(z) / ((1 + exp(z))^2)
}
linear_predictor1 = predict(fit1, newdata = data.frame(dist100 = avg_dist))
coef(fit1)[-1] * lambda(linear_predictor1)

##Add columns:
df = df %>%
  mutate(p1 = predict(fit1, type = 'response'),
         pred1 = 1 * (p1 > 0.5))

#Error rate of the Bayes classifier:
df %>%
  summarize(error_rate = mean((pred1 == 1 & switch == 0) | (pred1 == 0 & switch == 1)))

#Using larsenic to predict switch:
fit2 = glm(switch ~ log_arsenic, df, family = binomial(link = 'logit'))
fit2



#Using zeduc to predict switch:
fit3 = glm(switch ~ z_education, df, family = binomial(link = 'logit'))
fit3



#Using dist100, larsenic, and zeduc to pre-dict switch:
fit4 = glm(switch ~ dist100 + log_arsenic + z_education, df, family = binomial(link = 'logit'))
fit4


#Plots:
fit2_plot = ggplot(df, aes(x = log_arsenic, y = switch)) +
  geom_jitter(height = 0.15) +
  stat_smooth(method='glm',
              method.args = list(family = "binomial"),
              formula = y ~ x) +
  xlab('log Arsenic Concentration') +
  ylab('Prob')

fit3_plot = ggplot(df, aes(x = z_education, y = switch)) +
  geom_jitter(height = 0.15) +
  stat_smooth(method='glm',
              method.args = list(family = "binomial"),
              formula = y ~ x) +
  xlab('years of education (z-score)') +
  ylab('Prob')

grid.arrange(fit2_plot, fit3_plot, ncol = 2)


#Marginal effect of each predictor:
avg_log_arsenic = df %>%
  summarize(avg_log_arsenic = mean(log_arsenic)) %>%
  pull(avg_log_arsenic)
mean_household = data.frame(dist100 = avg_dist,
                            log_arsenic = avg_log_arsenic,
                            z_education = 0)
linear_predictor4 = predict(fit4, newdata = mean_household)
coef(fit4)[-1] * lambda(linear_predictor4)



#Error rate of the Bayes classifier:
df = df %>%
  mutate(p4 = predict(fit4, type = 'response'),
         pred4 = 1 * (p4 > 0.5))

df %>%
  summarize(error_rate = mean((pred4 == 1 & switch == 0) | (pred4 == 0 & switch == 1)))

#The error rate of fit4 is  0.3695 and the error rate of fit 1 is 0.4046. The fit 4 performs better.