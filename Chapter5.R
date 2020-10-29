rm(list = ls())
library(ISLR)
data(Auto)
fix(Auto)

#Lab for Validation Set Approach
set.seed(1)
train = sample(392, 196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
attach(Auto)

mean((mpg - predict(lm.fit, Auto))[-train]^2)

lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

set.seed(2)
train <- sample(392, 196)
lm.fit <- lm(mpg~horsepower, data = Auto, subset = train)
mean((mpg - predict(lm.fit, Auto))[-train]^2)
lm.fit2 <- lm(mpg~poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)
lm.fit3 <- lm(mpg~poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

#Lab for LOOCV
glm.fit <- glm(mpg~horsepower, data = Auto)
coef(glm.fit)
#when use general linear regression without using family=binomial, it is
#essentially a linear regression!

library(boot)
cv.err <- cv.glm(Auto, glm.fit)
names(cv.err) # "call"  "K"     "delta" "seed"
#here K = 392, K = n
cv.err$delta
#here delta has two values, first one is the estimate of MSE, second one is a bias-corrected version


cv.err <- rep(0, 5)
for (i in 1:5){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.err

set.seed(17)
#why do we need to set seed here? 
cv.err.10 <- rep(0, 10)
for (i in 1: 10){
  glm.fit <- glm(mpg~poly(horsepower, i), data = Auto)
  cv.err.10[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.err.10

#Lab for bootstrap

alpha.fn <- function(data, indrange) {
  X = data$X[indrange]
  Y = data$Y[indrange]
  return((var(Y) - cov(X, Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

alpha.fn(Portfolio, 1:100)

#one bootstrap example
set.seed(1)
alpha.fn(Portfolio, sample(100, 100, replace = T))

boot(Portfolio, alpha.fn, R = 1000)


#use another example of linear regression coef estimates
boot.fn <- function(data, indrange) {
  return(coef(lm(mpg~horsepower, data = Auto, subset = indrange)))
}

#same, calculate the statistic of interest
boot.fn(Auto, 1:392)

set.seed(1)
boot.fn(Auto, sample(392, 392, replace = T))
boot.fn(Auto, sample(392, 392, replace = T))

boot(Auto, boot.fn, 1000)

summary(lm(mpg~horsepower, data = Auto))$coef

#use polynomial regression to do it again

boot.fn <- function(data, indrange) {
  return(coef(glm(mpg~horsepower + I(horsepower^2), data=data, subset = indrange)))
}

set.seed(1)
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower + I(horsepower^2), data = Auto))$coef


store = rep(NA, 10000)
for (i in 1:10000) {
  store[i] = sum(sample(1:100, replace = T) == 4) > 0
}
store[1:30]
mean(store)