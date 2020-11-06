# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)
High=factor(ifelse(Sales<=8,"No","Yes"))
Carseats=data.frame(Carseats,High)
summary(Carseats)

tree.carseats <- tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty = 0, cex = 0.8)

tree.carseats

#use validation set to calculate test error rate
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
test <- -train
tree.carseats <- tree(High~.-Sales, data = Carseats[train, ])
tree.pred <- predict(tree.carseats, newdata = Carseats[test, ], type = "class")
table(tree.pred, High[test])

#use cv and prune tree
set.seed(3)
cv.carseats <-cv.tree(tree.carseats, FUN = prune.misclass)
names(cv.carseats)
cv.carseats

par(mfrow = c(1, 2))
plot(cv.carseats$size, cv.carseats$dev, type = "b")
plot(cv.carseats$k, cv.carseats$dev, type = "b")

prune.carseats <- prune.misclass(tree.carseats, best = 9)
plot(prune.carseats)
text(prune.carseats, pretty = 0)

tree.pred <- predict(prune.carseats, Carseats[test, ], type = "class")
table(tree.pred, High[test])
(97 + 58)/200

prune.carseats <- prune.misclass(tree.carseats, best = 15)
tree.pred <- predict(prune.carseats, Carseats[test, ], type = "class")
table(tree.pred, High[test])

#fitting a regression tree
set.seed(1)
library(MASS)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
test = -train

tree.boston <- tree(medv~., data = Boston[train, ])
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0)

#use cross-validation to choose the right size 
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev)

prune.boston <- prune.tree(tree.boston, best = 6)
plot(prune.boston)
text(prune.boston, pretty = 0)

yhat <- predict(tree.boston, newdatat = Boston[test, ])
plot(yhat, Boston[test, "medv"])
abline(0, 1)
mean((yhat - )^2)

#Bagging, Random Forest
set.seed(1)
library(randomForest)
bag.boston <- randomForest(medv~., data = Boston[train, ], mtry = 13, importance = TRUE)
bag.boston

yhat.bag <- predict(bag.boston, newdata = Boston[test, ])
plot(yhat.bag, Boston[test, "medv"])
abline(0, 1)
mean((yhat.bag - Boston$medv[test])^2)
mean((yhat.bag - Boston[test, "medv"])^2)

bag.boston <- randomForest(medv~., Boston[train, ], mtry = 13, ntree = 25)
yhat.bag <- predict(bag.boston, newdata = Boston[test, ])
plot(yhat.bag, Boston[test, "medv"])
abline(0, 1)
mean((yhat.bag - Boston$medv[test])^2)
#mean((yhat.bag - Boston[test, "medv"])^2)

#Random Forest
set.seed(1)
rf.boston <- randomForest(medv~., Boston[train, ], mtry = 6, importance = TRUE)
yhat.rf <- predict(rf.boston, Boston[test, ])
mean((yhat.rf - Boston$medv[test])^2)

importance(rf.boston)
varImpPlot(rf.boston)


#Boosting
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~., Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4)
summary(boost.boston)

par(mfrow = c(1,2))
plot(boost.boston, i = "rm")
plot(boost.boston, i = "lstat")

yhat.boost <- predict(boost.boston, Boston[test, ], n.trees = 5000)
mean((yhat.boost - Boston$medv[test])^2)

boost.boston <- gbm(medv~., Boston[train, ], distribution = "gaussian", n.trees = 5000, interaction.depth = 4, shrinkage = 0.2, verbose = F)
yhat.boost <- predict(boost.boston, Boston[test, ], n.trees = 5000)
mean((yhat.boost - Boston$medv[test])^2)
