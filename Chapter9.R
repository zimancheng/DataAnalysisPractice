library(e1071)

set.seed(1)
x = matrix(rnorm(20 *2), ncol = 2)
y <- c(rep(-1, 10), rep(1, 10))
x[y==1,] = x[y==1, ] + 1
x
plot(x)
plot(x, col = (3-y))
#col = 4 is blue, col = 2 is red

dat <- data.frame(x=x, y= as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10, scale = F)

plot(svmfit, dat)
#the support vectors are plotted as crosses and the remaining observations are plotted as circles

svmfit$index

summary(svmfit)

svmfit <- svm(y~., data = dat, kernel = "linear", cost = 0.1, scale = F)
plot(svmfit, dat)
svmfit$index

#cv the cost, i.e. the margin
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel = "linear", 
                 ranges = list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 100)))
summary(tune.out)

bestmod <- tune.out$best.model
summary(bestmod)
plot(bestmod, dat)

xtest <- matrix(rnorm(20*2), ncol = 2)
ytest <- sample(c(-1, 1), 20, rep = TRUE)
xtest[ytest == 1, ] = xtest[ytest==1, ] + 1
testdat <- data.frame(x = xtest, y = as.factor(ytest))
ypred <- predict(bestmod, testdat)
table(predict = ypred, truth = testdat$y)

svmfit <- svm(y~., data = dat, kernel = "linear", cost = .01, scale = F)
ypred <- predict(svmfit, testdat)
table(ypred, testdat$y)

#what if the two classes are linearly separable
x[y==1, ] = x[y==1,] + 0.5
plot(x, col = (y + 5)/2, pch = 19)

dat <- data.frame(x = x, y = as.factor(y))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 1e5)
summary(svmfit)
plot(svmfit, dat)
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 1)
summary(svmfit)
plot(svmfit, dat)

#Support Vector Machine
set.seed(1)
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100, ]=x[1:100,] + 2
x[101:150, ] = x[101:150, ] - 2
y = c(rep(1, 150), rep(2, 50))
dat <- data.frame(x = x, y = as.factor(y))
plot(x, col = y)

train <- sample(200, 100)
svmfit <- svm(y~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1)
plot(svmfit, dat[train, ])
summary(svmfit)

svmfit <- svm(y~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 1e5)
plot(svmfit, dat[train, ])

set.seed(1)
tune.out <- tune(svm, y~., data = dat[train, ], kernel = "radial",
                 ranges = list(cost = c(0.1, 1, 10, 100, 1000),
                               gamma = c(0.5, 1, 2, 3, 4)))
summary(tune.out)

table(true = dat[-train, "y"], pred = predict(tune.out$best.model, dat[-train, ]))

#ROC Curves
library(ROCR)
rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}

svmfit.opt <- svm(y~., data = dat[train, ], kernel = "radial", gamma = 2, cost = 1, decision.values = T)
fitted <- attributes(predict(svmfit.opt, dat[train, ], decision.values = T))$decision.values
par(mfrow = c(1,2))
rocplot(fitted, dat[train, "y"], main = "Training Data")

#SVM with Multiple Classes
set.seed(1)
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0, 50))           
x[y==0, 2] = x[y==0, 2] + 2
dat = data.frame(x = x, y = as.factor(y))
par(mfrow = c(1,1))
plot(x, col = y +1)

svmfit <- svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
plot(svmfit, dat)

#Application to Gene Expression Data
library(ISLR)
names(Khan)
#Khan is a list object, not a data frame

dim(Khan$xtrain)
length(Khan$ytest)
table(Khan$ytrain)

dat <- data.frame(x = Khan$xtrain, y = as.factor(Khan$ytrain))
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10)
summary(svmfit)
table(svmfit$fitted, dat$y)
table(svmfit$fitted, Khan$ytrain)

#test MSE
dat.test <- data.frame(x = Khan$xtest, y = as.factor(Khan$ytest))
pred <- predict(svmfit, newdata = dat.test)
table(pred, dat.test$y)
