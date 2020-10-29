rm(list = ls())

fix(Default)
dim(Default)
set.seed(1)

glm.fit <- glm(default~income + balance, data = Default, family = binomial)
summary(glm.fit)

attach(Default)
train <- sample(length(default), length(default)/2)

glm.fit2 <- glm(default~income + balance, data = Default, family = binomial,
                subset = train)
summary(glm.fit2)

glm.prod <- predict(glm.fit2, newdata = Default[-train, ], type = "response")
glm.pred <- rep("No", length(default)/2)
glm.pred[glm.prod > .5] <- "Yes"
table(glm.pred, Default[-train, ]$default)
mean(glm.pred != Default[-train, ]$default)

set.seed(2)
train <- sample(10000, 5000)
glm.fit <- glm(default~income + balance, data = Default, family = binomial, subset = train)
glm.prob <- predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred <- rep("No", 5000)
glm.pred[glm.prob > .5] <- "Yes"
mean(glm.pred != default[-train])

set.seed(3)
train <- sample(10000, 5000)
glm.fit <- glm(default~income + balance, data = Default, family = binomial, subset = train)
glm.prob <- predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred <- rep("No", 5000)
glm.pred[glm.prob > .5] <- "Yes"
mean(glm.pred != default[-train])


set.seed(3)
train <- sample(10000, 5000)
glm.fit <- glm(default~income + balance + student, data = Default, family = binomial, subset = train)
glm.prob <- predict(glm.fit, newdata = Default[-train, ], type = "response")
glm.pred <- rep("No", 5000)
glm.pred[glm.prob > .5] <- "Yes"
mean(glm.pred != default[-train])

#6
set.seed(1)
glm.fit <- glm(default~income + balance, data = Default, family = binomial)
summary(glm.fit)

boot.fn <- function(data, indrange) {
  return(coef(glm(default~income + balance, data = data, family = binomial, subset = indrange)))
}

boot(Default, boot.fn, 1000)

#7
glm.fit7 <- glm(Direction~Lag1 + Lag2, data = Weekly, family = binomial)
glm.fit7.1 <- glm(Direction~Lag1 + Lag2, data = Weekly[-1, ], family = binomial)
predict.glm(glm.fit7.1, Weekly[1, ], type = "response") #up
Weekly[1, ]$Direction# down
# prediction is not correct

error <- rep(0, dim(Weekly)[1])
for (i in 1: dim(Weekly)[1]) {
  glm.fiti <- glm(Direction~Lag1 + Lag2, data = Weekly[-i, ], family = binomial)
  pred.up <- predict.glm(glm.fiti, Weekly[i, ], type = "response") > 0.5
  true.up <- Weekly[i, ]$Direction == "Up"
  if (pred.up != true.up)
    error[i] = 1
}
error
mean(error)
#The LOOCV estimate for the test error rate is 44.9954086%

#8
set.seed(1)
x = rnorm(100)
y = x - 2 * x^2 + rnorm(100)
plot(x, y)
Data <- data.frame(x, y)

set.seed(100)
error <- rep(0, 4)
for (i in 1: 4) {
  glm.fit <- glm(y~poly(x, i))
  error[i] = cv.glm(Data, glm.fit)$delta[1]
}
error

#the results are identical since LOOCV basically evaluates n folds of the same observation set

summary(glm(formula = y~poly(x, 4)))

#9
library(MASS)
mu.hat <- mean(Boston$medv)
mu.hat 
attach(Boston)
se.hat <- sd(medv)/sqrt(dim(Boston)[1])

set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return(mu)
}
boot(medv, boot.fn, 1000)

t.test(medv)

CI.mu.hat <- c(22.53 - 2 * 0.41, 22.53 + 2 * 0.41)
CI.mu.hat

#use median to test bootstrap again
med.hat <- median(medv)
med.hat

set.seed(1)
boot.fn <- function(data, index) {
  return(median(data[index]))
}
boot(medv, boot.fn, 1000)

#now use tenth pencentile
set.seed(1)
percent.hat <- quantile(medv, c(0.1))
percent.hat

set.seed(1)
boot.fn <- function(data, index) {
  return(quantile(data[index], c(0.1)))
}
boot(medv, boot.fn, 1000)