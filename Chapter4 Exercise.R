rm(list = ls())

library(ISLR)
data(Weekly)
fix(Weekly)
dim(Weekly)

pairs(Weekly[, -9])

glm.fit <- glm(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = binomial)
summary(glm.fit)

glm.prob <- predict(glm.fit, Weekly, type = "response")
contrasts(Weekly$Direction)
glm.pred <- rep("Down", length(glm.prob))
glm.pred[glm.prob >.5] = "Up"
table(glm.pred, Weekly$Direction)
mean(glm.pred == Weekly$Direction)
(54+557)/1089

attach(Weekly)
Direction

train <- Year < 2009
Direction.test <- Direction[!train]
length(Direction.test)

glm.fit <- glm(Direction~Lag2, data = Weekly, family = binomial, subset = train)
summary(glm.fit)
glm.prob <- predict(glm.fit, Weekly[!train, ], type = "response")
glm.pred <- rep("Down", 104)
glm.pred[glm.prob > .5] = "Up"
table(glm.pred, Direction.test)
mean(glm.pred == Direction.test)
(56 + 9)/104


lda.fit <- lda(Direction~Lag2, data = Weekly, subset = train)
lda.pred <- predict(lda.fit, Weekly[!train, ])
table(lda.pred$class, Direction.test)

qda.fit <- qda(Direction~Lag2, data = Weekly, subset = train)
qda.pred <- predict(qda.fit, Weekly[!train, ])
table(qda.pred$class, Direction.test)


#using KNN when K = 1
train.X <- as.matrix(Lag2[train])
test.X <- as.matrix(Lag2[!train])
train.Y <- Direction[train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
table(knn.pred, Direction.test)


#11
data(Auto)
mpg
fix(Auto)
mpg <- Auto$mpg
mpg

attach(Auto)
mpg01 <- rep(0, length(mpg))
mpg01[mpg > median(mpg)] = 1
median(mpg)

Auto <- data.frame(Auto, mpg01)
cor(Auto[, -9])
Auto[, 9]
names(Auto)[9]

boxplot(horsepower~mpg01, data = Auto, main = "Horsepower vs. mpg01")
boxplot(year~mpg01, data = Auto, main = "Year vs. mpg")
boxplot(displacement~mpg01, data = Auto, main = "Displacement vs. mpg")

#12

power <- function() {
  print(2^3)
}
power
power()

Power2 <- function(x, a){
  print(x^a)
}

Power2(1, 2)
Power2(3, 6)
27^2

Power3 <- function(x, a){
  result <- x^a
  return(result)
}

Power3(3, 6)

x <- 1:10
plot(x, Power3(x, 2), log = "xy", xlab = "x", ylab = "x square", main = "x^2", col = "yellow")

plotpower <- function(x, a) {
  plot(x, Power3(x, a))
}
plotpower(1:10, 3)
