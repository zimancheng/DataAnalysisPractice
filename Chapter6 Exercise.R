set.seed(1)
x <- rnorm(100)
esp <- rnorm(100)

b0 <- 2
b1 <- 3
b2 <- -1
b3 <- 0.5
y <- b0 + b1*x + b2*x^2 + b3*x^3 + esp

library(leaps)
data.full <- data.frame(y = y, x = x)
reg.fit <- regsubsets(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) +I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10)
reg.summary <- summary(reg.fit)

par(mfrow = c(2, 2))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], cex = 2, pch = 20, col = "green")
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col = "green", cex = 2, pch = 20)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col = "green", cex = 2, pch = 20)
coef(reg.fit, 3)

#use forward stepwise selection
reg.fwd <- regsubsets(y~x + I(x^2) + I(x^3) + I(x^4) + I(x^5) +I(x^6) + I(x^7) + I(x^8) + I(x^9) + I(x^10), data = data.full, nvmax = 10, method = "forward")
fwd.summary <- summary(reg.fwd)

par(mfrow = c(2, 2))
plot(fwd.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
points(which.min(fwd.summary$cp), fwd.summary$cp[which.min(fwd.summary$cp)], col = "green", cex = 2, pch = 20)
plot(fwd.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
points(which.min(fwd.summary$bic), fwd.summary$bic[which.min(fwd.summary$bic)], col = "green", cex = 2, pch = 20)
plot(fwd.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsquares", type = "l")
points(which.max(fwd.summary$adjr2), fwd.summary$adjr2[which.max(fwd.summary$adjr2)], col = "green", cex = 2, pch = 20)
coef(reg.fwd, 3)

#use backward stepwise selection should be the same 

#e
library(glmnet)
xmat <- model.matrix(y~poly(x, 10, raw = T), data = data.full)[, -1]
cv.out <- cv.glmnet(xmat, y, alpha = 1)
lambda <- cv.out$lambda.min
lambda

plot(cv.out)
fit.lasso <- glmnet(xmat, y, alpha = 1)
predict(fit.lasso, s=lambda, type = "coefficients")

#f
b7 <- 7
y <- b0 + b7 * x^7 + esp
data.full <- data.frame(x = x, y = y)
regfit.full <- regsubsets(y~poly(x, 10, raw = T), data = data.full, nvmax = 10)
reg.summary <- summary(regfit.full)
par(mfrow = c(2, 2))
plot(reg.summary$cp, ylab = "Cp", type = "l")
cpmin <- which.min(reg.summary$cp)
points(cpmin, reg.summary$cp[cpmin], col = "red", cex = 2, pch = 20)
plot(reg.summary$bic, ylab = "BIC", type = "l")
bic.min <- which.min(reg.summary$bic)
points(bic.min, reg.summary$bic[bic.min], col = "red", cex = 2, pch = 20)
plot(reg.summary$adjr2, type = "l")
adjr.max <- which.max(reg.summary$adjr2)
points(adjr.max, reg.summary$adjr2[adjr.max], col = "red", cex = 2, pch = 20)

coef(regfit.full, 2)
coef(regfit.full, 1)
coef(regfit.full, 4)

#now do it with lasso
xmat <- model.matrix(y~poly(x, 10, raw = T))[, -1]
cv.out <- cv.glmnet(xmat, y, alpha = 1)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam

fit.lasso <- glmnet(xmat, y, alpha = 1)
predict(fit.lasso, s=bestlam, type = "coefficients")

#9
library(ISLR)
fix(College)
set.seed(11)

train <- sample(1:dim(College)[1], dim(College)[1]/2)
test <- -train
College.train <- College[train, ]
College.test <- College[test, ]

#b
lm.fit <- lm(Apps~., data = College.train)
lm.pred <- predict(lm.fit, College.test)
mean((lm.pred - College.test$Apps)^2) #1026096

#c
train.mat <- model.matrix(Apps~., data = College.train)
test.mat <- model.matrix(Apps~., data = College.test)
grid <- 10^seq(4, -2, length = 100)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
plot(cv.ridge)
bestlam <- cv.ridge$lambda.min
bestlam

fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
pred.ridge <- predict(fit.ridge, s=bestlam, newx = test.mat)
mean((pred.ridge - College.test$Apps)^2) #1026069

#d
fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
plot(cv.lasso)
bestlam <- cv.lasso$lambda.min

pred.lasso <- predict(fit.lasso, s = bestlam, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2) #1026036
predict(fit.lasso, s = bestlam, type = "coefficients")

#e
library(pls)
fit.pcr <- pcr(Apps~., data = College.train, scale = T, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2) #1867486

#f
fit.pls <- plsr(Apps~., data = College.train, scale = T, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, College.test, ncomp = 10)
mean((pred.pls - College.test$Apps)^2) #1031287

#g
test.avg <- mean(College.test$Apps)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
pls.r2
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
pcr.r2
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
lasso.r2
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2)/mean((test.avg - College.test$Apps)^2)
ridge.r2
names(summary(lm.fit))
summary(lm.fit)$r.squared
#conclusion: 

#10
set.seed(1)
x <- matrix(rnorm(1000* 20), 1000, 20)
b <- rnorm(20)
b[3] <- 0
b[4] <- 0
b[9] <- 0
b[19] <- 0
b[10] <- 0
esp <- rnorm(1000)
y <- x%*%b + esp

#b
train <- sample(seq(1000), 100, replace = FALSE)
test <- -train
x.train <- x[train, ]
x.test <- x[test, ]
y.train <- y[train]
y.test <- y[test]

#c
data.train <- data.frame(y = y.train, x = x.train)
regfit.best <- regsubsets(y~., data = data.train, nvmax = 20)
train.mat <- model.matrix(y~., data = data.train)
val.errors <- rep(NA, 20)
for (i in 1:20) {
  coefi = coef(regfit.best, i)
  pred = train.mat[, names(coefi)]%*%coefi
  val.errors[i] = mean((pred - y.train)^2)
}
val.errors
par(mfrow = c(1, 1))
plot(val.errors, xlab = "Number of Variables", ylab = "Training MSE", pch = 19, type = "b")

#d
data.test <- data.frame(y = y.test, x = x.test)
test.mat <- model.matrix(y~., data = data.test)
val.testMSE = rep(NA, 20)
for (i in 1:20) {
  coefi = coef(regfit.best, id = i)
  pred = test.mat[, names(coefi)]%*%coefi
  val.testMSE[i] = mean((pred - y.test)^2)
}
plot(val.testMSE, pch = 19, type = "b")

#e
which.min(val.testMSE)
regfit.best <- regsubsets(y~., data = data.frame(y = y, x = x), nvmax = 20)
coef(regfit.best, 15)
#the best subset using Validation Set caught all zeroed out coefficients

#g
val.errors <- rep(NA, 20)
x_cols = colnames(x, do.NULL = FALSE, prefix = "x.")
for (i in 1:20) {
  coefi <- coef(regfit.best, id = i)
  val.errors[i] <- sqrt(sum((b[x_cols %in% names(coefi)] - coefi[names(coefi) %in% x_cols])^2) + sum(b[!(x_cols %in% names(coefi))])^2)
}
plot(val.errors, xlab = "Number of coefficients", ylab = "Error between estimated and true coefficients", pch = 19, type = "b")

#11
library(MASS)
data("Boston")
fix(Boston)
dim(Boston)

#use Best Subset Selection with 10folds Cross Validation
k=10
set.seed(1)
folds <- sample(1:k, nrow(Boston), replace = TRUE)

predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  x.mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  x.mat[, names(coefi)]%*%coefi
}

cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  regfit.best <- regsubsets(crim~., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1: 13) {
    pred <- predict(regfit.best, Boston[folds == j, ], i)
    cv.errors[j, i] = mean((pred - Boston$crim[folds == j])^2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(mean.cv.errors, type = "b")
points(which.min(mean.cv.errors), mean.cv.errors[which.min(mean.cv.errors)], col = "red", cex = 2, pch = 20)

#use PCR
library(pls)
set.seed(1)
pcr.fit <- pcr(crim~., data = Boston, scale = TRUE, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

#use ridge
x <- model.matrix(crim~., data = Boston)[, -1]
y <- Boston$crim
grid <- 10^seq(10, -2, length = 100)
cv.out <- cv.glmnet(x, y, alpha = 0, lambda = grid, thresh = 1e-12)
fit.ridge <- glmnet(x, y, alpha = 0, lambda = grid, thresh = 1e-12)
plot(cv.out)
