#Subset Selection Methods 
library(ISLR)
fix(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
Hitters
dim(Hitters)
sum(is.na(Hitters))

library(leaps)
regfit.full <- regsubsets(Salary~., data = Hitters)
summary(regfit.full)

regfit.full <- regsubsets(Salary~., data = Hitters, nvmax = 19)
reg.summary <- summary(regfit.full)
names(reg.summary)
reg.summary$rsq

par(mfrow = c(2, 2))
plot(reg.summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")
which.max(reg.summary$adjr2)
points(11, reg.summary$adjr2[11], col = "red", cex = 3, pch = 20)

plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
which.min(reg.summary$cp)
points(10, reg.summary$cp[10], col = "green", cex = 2, pch = 20)

plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
which.min(reg.summary$bic)
points(6, reg.summary$bic[6], col = "orange", cex = 2, pch = 1)

dev.off()
par(mfrow = c(1, 2))
plot(regfit.full, scale = "r2")
plot(regfit.full, scale = "adjr2")
dev.off()
plot(regfit.full, scale = "bic")
dev.off()

coef(regfit.full, 6)
#choose the 6-variable model to be our final selection based on BIC statistic.

#Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "forward")
summary(regfit.fwd)
regfwd.summary <- summary(regfit.fwd)
plot(regfwd.summary$bic, type = "l")
which.min(regfwd.summary$bic)
points(6, regfwd.summary$bic[6], col = "green", cex = 2, pch = 20)
coef(regfit.fwd, 7)
coef(regfit.full, 7)

regfit.bwd <- regsubsets(Salary~., data = Hitters, nvmax = 19, method = "backward")
regbwd.summary <- summary(regfit.bwd)
plot(regfit.bwd, scale = "bic")
coef(regfit.bwd, 7)

#Using Validation Set to choose the best subset model
set.seed(1)
train <- sample(c(TRUE, FALSE), nrow(Hitters), replace = T)
test <- (!train)

regfit.best <- regsubsets(Salary~., data = Hitters[train, ], nvmax = 19)
summary(regfit.best)
test.mat <- model.matrix(Salary~., data = Hitters[test, ])
test.mat

val.errors <- rep(NA, 19)
for (i in 1:19){
  coefi <- coef(regfit.best, id = i)
  pred <- (test.mat[, names(coefi)])%*%coefi
  val.errors[i] <- mean((Hitters$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)

coef(regfit.best, 7)

#setup our own predict() method for regsubsets()
predict.regsubsets <- function(object, newdata, id, ...) {
  form <- as.formula(object$call[[2]])
  mat <- model.matrix(form, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)]%*%coefi
}

regfit.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(regfit.best, 10)

#use Cross Validation to do Best Subset Selection
k = 10
set.seed(1)
folds = sample(1:k, nrow(Hitters), replace = T)
cv.errors = matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))
cv.errors

for (i in 1:k) {
  best.fit <- regsubsets(Salary~., data = Hitters[folds != i, ], nvmax = 19)
  for (j in 1:19) {
    pred = predict(best.fit, Hitters[folds == i, ], j)
    cv.errors[i, j] = mean((Hitters$Salary[folds == i] - pred)^2)
  }
}
cv.errors

mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors
par(mfrow = c(1, 1))
plot(mean.cv.errors, type = "b")

reg.best <- regsubsets(Salary~., data = Hitters, nvmax = 19)
coef(reg.best, 11)

#Lab 2: Ridge Regression and the Lasso
x <- model.matrix(Salary~., Hitters)[, -1] #removed the intercept term
y <- Hitters$Salary

library(glmnet)
grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)

coef(ridge.mod)
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[, 50]
sqrt(sum(coef(ridge.mod)[-1, 50]^2)) #note -1 is important, we have to remove the intercept
#to calculate the l2 norm of coefficients

ridge.mod$lambda[60]
sqrt(sum((coef(ridge.mod)[-1, 60])^2))

#we can use predict to obtain the coefficient estimates of a lambda
predict(ridge.mod, s = 50, type = "coefficients")[1:20, ]
#here [1:20, ] takes the original 20*1 matrix and displays it as a 1*20 vector

#Use cross validation to choose the lambda
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- -train
y.test <- y[test]

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
ridge.pred <- predict(ridge.mod, s = 1e10, newx = x[test, ])
mean((ridge.pred-y.test)^2)
mean((mean(y[train])- y.test)^2)
ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ])
mean((ridge.pred - y.test)^2)
lm(y~x, subset = train)
predict(ridge.mod, s=0, type = "coefficients")[1:20, ]

#instead of randomly choosing lambda = 4, we use cv.glmnet to choose lambda
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

ridge.pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((ridge.pred - y.test)^2)

out <- glmnet(x, y, alpha = 0)
predict(out, type = "coefficients", s=bestlam)[1:20, ]

#lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

lasso.pred <- predict(lasso.mod, s=bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2)

out <- glmnet(x, y, alpha = 1)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef

#PCR and PLS Regression
library(pls)
set.seed(2)
pcr.fit <- pcr(Salary~., data = Hitters, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
validationplot(pcr.fit, val.type = "MSEP")

pcr.pred <- predict(pcr.fit, x[test, ], ncomp = 7)
mean((pcr.pred - y.test)^2)

pcr.fit <- pcr(y~x, scale = T, ncomp = 7)
summary(pcr.fit)

#pls
set.seed(1)
pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")

pls.pred <- predict(pls.fit, x[test, ], ncomp = 2)
mean((pls.pred - y.test)^2)

pls.fit <- plsr(y~x, scale = T, ncomp = 2)
summary(pls.fit)
