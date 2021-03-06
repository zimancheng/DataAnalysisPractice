library(MASS)
library(ISLR)
dim(Boston)

#3.6.2 Simple Linear Regression
"The objective is to find the linear regression of medv and lstate.
"
attach(Boston)

lm.fit = lm(medv~lstat, data=Boston)
lm.fit
summary(lm.fit)
names(lm.fit)

coef(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval = "confidence")
plot(lstat, medv)
abline(lm.fit)

# abline(lm.fit, lwd = 3)
# abline(lm.fit, lwd = 3, col="red")
# plot(lstat, medv, col="red")
# plot(lstat, medv, pch = 20)
# plot(lstat, medv, pch = "+")
# plot(1:20, 1: 20, pch = 1:20)

par(mfrow=c(2, 2))
plot(lm.fit)

plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
dev.off()

#3.6.3 Multiple Linear Regression
rm(list = ls())
lm.fit <- lm(medv~lstat + age, data = Boston)
summary(lm.fit)
summary(lm.fit)$r.sq
summary(lm.fit)$sigma

lm.fit <- lm(medv~.-age, data = Boston)
summary(lm.fit)
library(car)
vif(lm.fit)
#lm.fit1 <- update(lm.fit, ~.-age)


#3.6.4 Interation
lm.fit <- lm(medv~lstat + age + lstat: age, data = Boston)
summary(lm.fit)
# lm.fit <- lm(medv~lstat*age, data = Boston)

# 3.6.5 Non-linear Transformation of the Predictors
lm.fit <- lm(medv~lstat, data = Boston)
lm.fit2 <- lm(medv~lstat + I(lstat^2), data = Boston)
summary(lm.fit2)

anova(lm.fit, lm.fit2)
# proves that lm.fit2 with the quadratic term of lstat is better

par(mfrow = c(2, 2))
plot(lm.fit2)

lm.fit3 <- lm(medv~poly(lstat, 5))
summary(lm.fit3)

lm.fit4 <- lm(medv~log(lstat))
summary(lm.fit4)

#3.6.6 Qualitative Predictors
fix(Carseats)
str(Carseats)

attach(Carseats)
lm.fitcar <- lm(Sales~.+ Income: Advertising + Price: Age, data = Carseats)
summary(lm.fitcar)
contrasts(ShelveLoc)

# 3.6.7 Functions
loadlibraries = function(){
  library(MASS)
  library(ISLR)
  print("The libraries have been loaded!!!")
}
loadlibraries
loadlibraries()
