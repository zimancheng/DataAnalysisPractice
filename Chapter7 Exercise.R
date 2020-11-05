library(ISLR)
attach(Wage)

#6 (a)
library(boot)
set.seed(1)
deltas <- rep(NA, 10)

for (i in 1:10) {
  fit <- glm(wage~poly(age, i), data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(1:10, deltas, xlab = "Degree", ylab = "Test MSE", type = "l")
min.degree <- which.min(deltas)
points(min.degree, deltas[min.degree], col = "red", cex = 2, pch = 20)

plot(age, wage, col = "darkgrey", cex = 0.5)
agelims = range(age)
age.grid = seq(agelims[1], agelims[2])
fit <- lm(wage~poly(age, 4), data = Wage)
pred = predict(fit, newdata = list(age = age.grid), se = T)
lines(age.grid, pred$fit, col = "blue", lwd = 2)
se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
matlines(age.grid, se.bands, lwd = 2, lty = 3, col = "blue")

#6b
deltas <- rep(NA, 10)
for (i in 2: 10) {
  Wage$age.cut <- cut(Wage$age, i)
  fit <- glm(wage~age.cut, data = Wage)
  deltas[i] <- cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, deltas[-1], type = "l")
min.cut <- which.min(deltas)
points(min.cut, deltas[min.cut], cex = 2, pch = 20, col = "red")

fit <- lm(wage~cut(age, 8), data = Wage)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
plot(age, wage, cex = .5, col = "darkgrey")
lines(age.grid, pred$fit, col = "green", lwd = 2)


#7
set.seed(1)
summary(Wage$maritl)
summary(Wage$jobclass)

par(mfrow = c(1,2))
plot(maritl, wage)
plot(jobclass, wage)

#use smooth splines in GAM to fit the models 
library(gam)
gam.m1 <- gam(wage~lo(year, span = 0.7) + s(age, 5) + education, data = Wage)
gam.m2 <- gam(wage~lo(year, span = 0.7) + s(age, 5) + education + maritl, data = Wage)
gam.m3 <- gam(wage~lo(year, span = 0.7) + s(age, 5) + education + jobclass, data = Wage)
gam.m4 <- gam(wage~lo(year, span = 0.7) + s(age, 5) + education + maritl + jobclass, data = Wage)
anova(gam.m1, gam.m2, gam.m3, gam.m4)

par(mfrow = c(2, 3))
plot(gam.m4, se = T, col = "green")

#8
#look into wage on diplacement
set.seed(1)
attach(Auto)

#polynomials
deltas <- rep(NA, 15)
for (i in 1:15) {
  fit <- glm(mpg~poly(displacement, i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
par(mfrow = c(1, 1))
plot(1:15, deltas, type = "l")
min.degree <- which.min(deltas)
points(min.degree, deltas[min.degree], cex = 2, pch = 20, col = "red")

#step functions
deltas <- rep(NA, 10)
for (i in 2:10) {
  Auto$displacement.cut <- cut(Auto$displacement, i)
  fit <- glm(mpg~displacement.cut, data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(2:10, deltas[-1], type = "l")
min.cut <- which.min(deltas)
points(min.cut, deltas[min.cut], col = "red", cex = 2, pch = 20)

#splines
library(splines)
deltas <- rep(NA, 10)
for (i in 3:10) {
  fit <- glm(mpg~ns(displacement, df=i), data = Auto)
  deltas[i] <- cv.glm(Auto, fit, K = 10)$delta[1]
}
plot(3:10, deltas[c(-1, -2)], type = "l")
min.cut <- which.min(deltas)
points(min.cut, deltas[min.cut], col = "red", cex = 2, pch = 20)


#GAMs with splines
library(gam)
fit <- gam(mpg~s(displacement, df = 4) + s(horsepower, 4), data = Auto)
par(mfrow = c(1, 2 ))
plot(fit, col = "red", se = T)

#9
library(MASS)
attach(Boston)

#a
fit <- lm(nox~poly(dis, 3), data = Boston)
summary(fit)
dislims <- range(dis)
dis.grid <- seq(dislims[1], dislims[2])
pred <- predict(fit, newdata = data.frame(dis = dis.grid), se = T)
se.bands <- cbind(pred$fit + 2 * pred$se.fit, pred$fit - 2 * pred$se.fit)
par(mfrow=c(1, 1))
plot(dis, nox, cex= .5, col = "darkgrey")
lines(dis.grid, pred$fit, col = "green", lwd = 2)
matlines(dis.grid, se.bands, col = "green", lwd = 2, lty = 3)

#b
rss <- rep(NA, 10)
for (i in 1:10) {
  fit <- lm(nox~poly(dis, i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(1:10, rss, type = "l")

#c
deltas <- rep(NA, 10)
for (i in 1:10) {
  fit <- glm(nox~poly(dis, i), data = Boston)
  deltas[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(1:10, deltas, type = "l")
which.min(deltas)

#d
fit <- lm(nox~bs(dis, df = 4), data = Boston)
plot(dis, nox, cex = .5, col = "darkgrey")
pred <- predict(fit, newdata = data.frame(dis = dis.grid), se = T)
lines(dis.grid, pred$fit, col = "green", lwd = 2)

#e
rss <- rep(NA, 16)
for (i in 3:16) {
  fit <- lm(nox~bs(dis, df = i), data = Boston)
  rss[i] <- sum(fit$residuals^2)
}
plot(3:16, rss[c(-1, -2)], type = "l")

#f
cv <- rep(NA, 16)
for (i in 3:16) {
  fit <- glm(nox~bs(dis, df = i), data = Boston)
  cv[i] <- cv.glm(Boston, fit, K = 10)$delta[1]
}
plot(3:16, cv[c(-1, -2)], type = "l")

#10
#use forward stepwise selection
library(leaps)
attach(College)
dim(College)
College = na.omit(College)
dim(College)
fix(College)

#train set and test set
set.seed(1)
train <- sample(1:nrow(College), nrow(College)/2)
test <- -train
regfit.best <- regsubsets(Outstate~., data = College[train, ], nvmax = 17, method = "forward")
summary(regfit.best)

#test errors
test.mat <- model.matrix(Outstate~., data = College[test, ])
val.errors <- rep(NA, 17)
for (i in 1:17) {
  coefi <- coef(regfit.best, id = i)
  pred <- test.mat[, names(coefi)]%*%coefi
  val.errors[i] <- mean((pred - College$Outstate[test])^2)
}
k <- which.min(val.errors)
k

fit.summary <- summary(regfit.best)
plot(fit.summary$bic)
points(which.min(fit.summary$bic), fit.summary$bic[which.min(fit.summary$bic)], col = "red", cex = 2, pch = 20)
#from bic, the best model is 6
#from validation set, the best model is 13
regfit.best <- regsubsets(Outstate~., data = College, nvmax = 17)
coefi <- coef(regfit.best, 6)
coefi

#10b use GAM
fit <- gam(Outstate~Private + s(Room.Board, df = 2) + s(Terminal, df = 2) + s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), data = College[train,])
par(mfrow = c(2, 3))
plot(fit, se = T, col = "blue")
anova(fit)
summary(fit)

#evaluate the model by R squared
pred <- predict(fit, College[test, ])
err <- mean((College$Outstate[test] - pred)^2)
err
tss <- mean((College$Outstate[test] - mean(College$Outstate[test]))^2)
rss <- 1 - err/tss
rss

#11
set.seed(1)
x1 <- rnorm(100)
x2 <- rnorm(100)
b0 <- -3.8
b1 <- 0.3
b2 <- 4.1
esp <- rnorm(100, sd = 1)
y <- b0 + b1 * x1 + b2*x2 + esp
par(mfrow = c(1, 1))
plot(y)

#cd
bhat1 <- 1
a <- y - bhat1 * x1
bhat2 <- lm(a~x2)$coef[2]

a <- y - bhat2*x2
bhat1 <- lm(a~x1)$coef[2]

#e
bhat0 <- bhat1 <- bhat2 <- rep(0, 1000)
for (i in 1:1000) {
  a <- y - bhat1[i]*x1
  bhat2[i] <- lm(a~x2)$coef[2]
  a <- y - bhat2[i]*x2
  bhat1[i + 1] <- lm(a~x1)$coef[2]
  bhat0[i] <- lm(a~x1)$coef[1]
}
require(ggplot2)
require(reshape2)
mydf <- data.frame(Iteration=1:1000, bhat0, bhat1=bhat1[-1], bhat2)
mmydf <- melt(mydf, id.vars = "Iteration")
ggplot(mmydf, aes(x=Iteration, y=value, group=variable, col=variable)) + geom_line(size = 1) + ggtitle("Plot of beta estimates by iteration")


