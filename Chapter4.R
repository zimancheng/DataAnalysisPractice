library(ISLR)
data(Smarket)
dim(Smarket)
cor(Smarket)
cor(Smarket[, -9])
attach(Smarket)
plot(Volume)

#4.6.2 Logistic Regression
glm.fit <- glm(Direction~.-Today-Year, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[,4]

glm.prob <- predict(glm.fit, type = "response")
glm.prob[1:20]

glm.pred <- rep("Down", 1250)
glm.pred[glm.prob >.5] = "Up"
glm.pred[1:20]

table(glm.pred, Direction)
(145 + 507)/1250 #0.5216
mean(glm.pred == Direction) #0.5216

#Set up training and test data set
train = Year < 2005
train
dim(train) #incorrect usage of dim
length(train)

Smarket.2005 <- Smarket[!train, ]
Direction.2005 <- Direction[!train] #!!!!  here direction is a vector hence don't use [!train, ]
glm.fit <- glm(Direction~.-Today-Year, data = Smarket, family = binomial, subset = train)
summary(glm.fit)

glm.prob <- predict(glm.fit, Smarket.2005, type = "response")
glm.prob[1:20]
glm.pred <- rep("Down", 252)
glm.pred[glm.prob >.5] = "Up"
glm.pred

table(glm.pred, Direction.2005)
(77+44)/252 # 0.48
mean(glm.pred == Direction.2005)

# use less preditors
glm.fit <- glm(Direction~Lag1 + Lag2, data = Smarket, family = binomial, subset = train)
summary(glm.fit)
glm.prob <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.prob >.5] = "Up"

table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # 1 - total error rate
106/(106 + 76) 

predict(glm.fit, newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)), type = "response")


# LDA
library(MASS)
lda.fit <- lda(Direction~Lag1 + Lag2, data = Smarket, subset = train)
lda.fit

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.pred
plot(lda.fit)

lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[, 1] >.5)
sum(lda.pred$posterior[, 2] < 0.5)
sum(lda.pred$posterior[, 1] >.9)


#QDA
qda.fit <- qda(Direction~Lag1 + Lag2, data = Smarket, subset = train)
qda.fit

qda.pred <- predict(qda.fit, Smarket.2005)
qda.class <- qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)

# KNN
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
test.X <- cbind(Lag1, Lag2)[!train, ]
train.Direction = Direction[train]
#test.direction, but we already got that, Direction.2005

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

knn.pred <- knn(train.X, test.X, train.Direction, k = 3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# Caravan Example
dim(Cavaran)
library(ISLR)
data("Caravan")
dim(Caravan)
attach(Caravan)

summary(Purchase)
348/5822

standarlized.X = scale(Caravan[, -86])
var(Caravan[, 1])
var(Caravan[, 2])
var(standarlized.X[, 1])
var(standarlized.X[, 2])

test = 1:1000
train.X <- standarlized.X[-test, ]
test.X <- standarlized.X[test, ]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k = 1)
mean(knn.pred == test.Y)
contrasts(knn.pred)
table(knn.pred, test.Y)

#use logistic regression on Caravan
glm.fit <- glm(Purchase~., data = Caravan, family = binomial, subset = -test)
glm.prob <- predict(glm.fit, Caravan[test, ], type = "response")
glm.pred = rep("No", 1000) #predict always computes the probability of P(Y=1|X)
#so when glm.prob > 0.5, it classifies Y to "Yes", but how do we know how it's coded?
contrasts(Purchase)
glm.pred[glm.prob > 0.5] = "Yes"
table(glm.pred, test.Y)

glm.pred <- rep("No", 1000)
glm.pred[glm.prob > 0.25] = "Yes"
table(glm.pred, test.Y)
11/(22 + 11)
