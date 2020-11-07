#1

par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(0,100), ylim = c(0,100), xlab = "X", ylab = "Y")
# t1: x = 40; (40, 0) (40, 100)
lines(x = c(40,40), y = c(0,100))
text(x = 40, y = 108, labels = c("t1"), col = "red")
# t2: y = 75; (0, 75) (40, 75)
lines(x = c(0,40), y = c(75,75))
text(x = -8, y = 75, labels = c("t2"), col = "red")
# t3: x = 75; (75,0) (75, 100)
lines(x = c(75,75), y = c(0,100))
text(x = 75, y = 108, labels = c("t3"), col = "red")
# t4: x = 20; (20,0) (20, 75)
lines(x = c(20,20), y = c(0,75))
text(x = 20, y = 80, labels = c("t4"), col = "red")
# t5: y=25; (75,25) (100,25)
lines(x = c(75,100), y = c(25,25))
text(x = 70, y = 25, labels = c("t5"), col = "red")

text(x = (40+75)/2, y = 50, labels = c("R1"))
text(x = 20, y = (100+75)/2, labels = c("R2"))
text(x = (75+100)/2, y = (100+25)/2, labels = c("R3"))
text(x = (75+100)/2, y = 25/2, labels = c("R4"))
text(x = 30, y = 75/2, labels = c("R5"))
text(x = 10, y = 75/2, labels = c("R6"))

#3
p <- seq(0, 1, 0.01)
gini <- 2 * p * (1 - p)
entropy <- -p*log(p) - (1 - p)*log(1- p)
class.err <- 1 - pmax(p, 1- p)
dev.off()
matplot(p, cbind(gini, entropy, class.err), col = c("red", "blue", "green"))

#4
dev.off()
par(xpd = NA)
plot(NA, NA, type = "n", xlim = c(-2, 2), ylim = c(-3, 3), xlab = "X1", ylab = "X2")
lines(x = c(-2, 2), y = c(1,1))
lines(x = c(-2,2), y = c(2,2 ))
lines(x = c(1, 1), y = c(-3,1))
lines(x = c(0, 0), y = c(1, 2))
text(x=0, y = (2+ 3)/2, labels = c(2.49))
text(x = -1, y = 1.4, labels = c(-1.06))
text(x = 1, y = 1.5, labels = c(0.21))
text(x = -1.5, y = -1.5, labels = c(-1.80))
text(x = 1.5, y = -1.5, labels = c(0.63))

#7
library(MASS)
library(randomForest)
fix(Boston) #medv col14
p <- ncol(Boston) - 1
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
test <- -train
x.train <- Boston[train, -14]
y.train <- Boston[train, "medv"]
x.test <- Boston[test, -14]
y.test <- Boston[test, "medv"]

rf.boston1 <- randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = p, ntree = 500)
rf.boston2 <- randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = log(p), ntree = 500)
rf.boston3 <- randomForest(x.train, y.train, xtest = x.test, ytest = y.test, mtry = p/2, ntree = 500)

plot(1:500, rf.boston1$test$mse, xlab = "Number of Trees",
     ylab = "test MSE", col = "green", type = "l",ylim = c(15, 35))
lines(1:500, rf.boston2$test$mse, col = "red", type = "l")
lines(1:500, rf.boston3$test$mse, col = "blue", type = "l")
legend("topright", c("m = p", "m = log(p)", "m = p/2"), col = c("green", "red", "blue"), cex = 1, lty = 1)

#8
library(ISLR)
library(tree)
fix(Carseats)
dim(Carseats)

set.seed(1)
train <- sample(1:nrow(Carseats), nrow(Carseats)/2)
test <- -train

tree.carseats <- tree(Sales~., Carseats[train, ])
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = .7)

pred <- predict(tree.carseats, Carseats[test, ])
mean((pred - Carseats[test, "Sales"])^2)

#8c
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")

prune.carseats <- prune.tree(tree.carseats, best = 7)
pred <- predict(prune.carseats, Carseats[test, ])
mean((pred - Carseats[test, "Sales"])^2)

#8d
bag.carseats <- randomForest(Sales ~ ., data = Carseats[train, ], mtry = 10, ntree = 500, importance = TRUE)
pred <- predict(bag.carseats, newdata = Carseats[test, ])
mean((pred - Carseats[test, "Sales"])^2)

importance(bag.carseats)
varImpPlot(bag.carseats)
#We may conclude that "Price" and "ShelveLoc" are the two most important variables.

#8e
rf.carseats <- randomForest(Sales~., Carseats[train, ], mtry = 3, ntree = 500, importance = TRUE)
pred <- predict(rf.carseats, newdata = Carseats[test, ])
mean((pred - Carseats[test, "Sales"])^2)
importance(rf.carseats)

#9
fix(OJ)
train <- sample(1:nrow(OJ), 800)
test <- -train

tree.oj <- tree(Purchase~., OJ[train, ])
summary(tree.oj)
#The fitted tree has 7 terminal nodes and a training error rate of 0.1737.

tree.oj
#We pick the node labeled 10, which is a terminal node because of the asterisk.
#The split criterion is SalePriceMM < 2.04, the number of observations in that branch is 103 with a deviance of 101.40 and an overall prediction for the branch of MM. 

#9d-h
plot(tree.oj)
text(tree.oj, pretty = 0)
pred <- predict(tree.oj, newdata = OJ[test, ], type =  "class")
table(pred, OJ[test, "Purchase"])
1 - (131 + 92)/270 #0.1740741

set.seed(2)
cv.oj <- cv.tree(tree.oj, FUN = prune.misclass)
par(mfrow = c(1,2 ))
plot(cv.oj$size, cv.oj$dev, type = "b")
plot(cv.oj$k, cv.oj$dev, type = "b")
#2 nodes tree is the best

dev.off()
plot(cv.oj$size, cv.oj$dev, type = "b")

#9i-k
prune.oj <- prune.misclass(tree.oj, best = 2)
summary(prune.oj)
#train error rates: 0.1888

#test error rate: a little higher than unpruned tree
pred <- predict(prune.oj, newdata = OJ[test, ], type = "class")
table(pred, OJ[test, "Purchase"])
(27 + 26)/270 #0.1962963

#10
library(gbm)
set.seed(1)
Hitters <- na.omit(Hitters)
Hitters$Salary <- log(Hitters$Salary)

train <- 1:200
test <- -train
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
train.error <- rep(NA, length(lambdas))

for (i in 1: length(lambdas)) {
  boost.hitter <- gbm(Salary~., data = Hitters[train, ], distribution = "gaussian", 
                      n.trees = 1000, interaction.depth = 4, shrinkage = lambdas[i])
  pred <- predict(boost.hitter, Hitters[train, ], n.trees = 1000)
  train.error[i] <- mean((pred - Hitters[train, "Salary"])^2)
}
plot(lambdas, train.error, xlab = "shrinkage parameter", ylab = "training MSE", type = "b")

test.error <- rep(NA, length(lambdas))
for (i in 1: length(lambdas)) {
  boost.hitter <- gbm(Salary~., data = Hitters[train, ], distribution = "gaussian", 
                      n.trees = 1000, interaction.depth = 4, shrinkage = lambdas[i])
  pred <- predict(boost.hitter, Hitters[test, ], n.trees = 1000)
  test.error[i] <- mean((pred - Hitters[test, "Salary"])^2)
}
plot(lambdas, test.error, xlab = "shrinkage parameter", ylab = "test MSE", type = "b")

#f
boost.hitter <- gbm(Salary~., data = Hitters[train, ], distribution = "gaussian", n.trees = 1000, interaction.depth = 4, 
                    shrinkage = lambdas[which.min(test.error)])
summary(boost.hitter)

#g
bag.hitter <- randomForest(Salary~., data = Hitters[train, ], mtry = 19, ntree = 500)
pred <- predict(bag.hitter, Hitters[test, ])
mean((pred - Hitters[test, "Salary"])^2)

#11
fix(Caravan)
Caravan$Purchase
dim(Caravan)

train <- 1:1000
test <- -train

boost.caravan <- gbm(Purchase~., Caravan[train, ], distribution = "gaussian", n.trees = 1000, shrinkage = 0.01, interaction.depth = 4)
summary(boost.caravan)
#The variables "PPERSAUT" and "PBRAND" are the two most important variables.

probs.test <- predict(boost.caravan, Caravan[test, ], n.trees = 1000, type = "response")
pred.test <- ifelse(probs.test > 0.2, 1, 0)
table(Caravan$Purchase[test], pred.test)
#not correct!