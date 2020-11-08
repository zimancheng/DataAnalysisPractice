#1
x1 <- -10:10
x2<- 1 + 3*x1
plot(x1, x2, type = "l", col = "blue")
text(c(0), c(20), "less than 0", col = "blue")
text(c(0), c(-20), "greater than 0", col = "blue")
lines(x1, 1-x1/2, col = "red")
text(c(0), c(15), "greater than 0", col = "red")
text(c(0), c(-15), "less than 0", col = "red")

#2
dev.off()
plot(NA, NA, xlab = "X1", ylab = "X2", asp = 1, xlim = c(-4, 2), ylim = c(-1,5))
symbols(c(-1), c(2), circles = c(2), add = TRUE, inches = FALSE)
text(c(-1), c(2), "<4")
text(c(-4), c(2), ">= 4")

#2c
plot(c(0, -1, 2, 3), c(0, 1, 2, 8), col = c("blue", "red", "blue", "blue"), 
     type = "p", asp = 1, xlab = "X1", ylab = "X2")
symbols(c(-1), c(2), circles = c(2), add = T, inches = F)

#3
x1 <- c(3, 2, 4, 1, 2, 4, 4)
x2 <- c(4, 2, 4, 4, 1, 3, 1)
colors <- c(2, 2, 2, 2, 4, 4, 4)
plot(x1, x2, col = colors, xlim = c(0, 5), ylim = c(0, 5))
abline(-0.5, 1)
abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#4
#set up dataset
library(e1071)
set.seed(1)
x1 <- rnorm(100)
x2 <- 4 *x1^2 + 1 + rnorm(100)
class <- sample(100, 50)
x2[class] = x2[class] - 3
x2[-class] = x2[-class] + 3
plot(x1[class], x2[class], col = "red", pch = 19, xlim = c(-3, 3), ylim = c(-6, 30))
points(x1[-class], x2[-class], col = "blue", pch = 19)

y <- rep(1, 100)
y[class] <- -1
dat <- data.frame(x = cbind(x1, x2), y = as.factor(y))
train <- sample(100, 50)

#fit the data with support vector classifier
svcfit <- svm(y~., data = dat[train, ], kernel = "linear", cost = 10)
summary(svcfit)
plot(svcfit, dat[train, ])
#test error rate
pred <- predict(svcfit, newdata = dat[-train, ])
table(pred, dat[-train, "y"])
#test error rate 8/50 = 0.4

#fit data with SVM with polynomial kernels
svmfit <- svm(y~., data = dat[train, ], kernel = "polynomial", cost = 10)
summary(svmfit)
plot(svmfit, dat[train, ])
#test error rate
pred <- predict(svmfit, newdata = dat[-train, ])
table(pred, dat[-train, "y"]) 
#test error rate for cubic svm: 7/50

#fit with SVM with radial kernels
svmfit <- svm(y~., data = dat[train, ], kernel = "radial", gamma = 1, cost = 10)
summary(svmfit)
plot(svmfit, dat[train,])
#test error rate
pred <- predict(svmfit, newdata = dat[-train, ])
table(pred, dat$y[-train]) #6/50 the best!!!!!

#5
set.seed(1)
x1 <- runif(500) - 0.5
x2 <- runif(500) - 0.5
y <- 1*(x1^2 - x2^2 > 0)
plot(x1, x2, xlab = "X1", ylab = "X2", col = 4 - y, pch = 3 -y)

log.fit <- glm(y~x1 + x2, family = "binomial")
summary(log.fit)
#None of the variables are statistically significants.

#5d
dat <- data.frame(x1 = x1, x2 = x2, y = as.factor(y))
prob <- predict(log.fit, dat, type = "response")
pred <- rep(0, 500)
pred[prob > 0.47] <- 1
pred
plot(dat[pred==1, ]$x1, dat[pred==1, ]$x2, col = 4 - 1, pch = 3 - 1)
points(dat[pred==0, ]$x1, dat[pred==0, ]$x2, col = 4, pch = 3)

#5ef
log.fit <- glm(y~poly(x1, 2) + poly(x2, 2) + I(x1*x2), family = "binomial")
prob <- predict(log.fit, dat, type = "response")
pred <- rep(0, 500)
pred[prob > 0.47] <- 1
pred
plot(dat[pred==1, ]$x1, dat[pred==1, ]$x2, col = 4 - 1, pch = 3 - 1)
points(dat[pred==0, ]$x1, dat[pred==0, ]$x2, col = 4, pch = 3)

#5g
svcfit <- svm(y~x1 + x2, data = dat, kernel = "linear", cost = 0.01)
plot(svcfit, dat)
pred <- predict(svcfit, dat)
plot(dat[pred==0, ]$x1, dat[pred==0, ]$x2, col = 4, pch = 3)
points(dat[pred==1, ]$x1, dat[pred==1, ]$x2, col = 3, pch = 2)

#5h
svmfit <- svm(y~., dat, kernel = "radial", gamma = 1)
plot(svmfit, dat)
pred <- predict(svmfit, dat)
plot(dat[pred==0, ]$x1, dat[pred==0, ]$x2, col = 4, pch = 3)
points(dat[pred==1, ]$x1, dat[pred==1, ]$x2, col = 3, pch = 2)

#6a
set.seed(1)
class1 <- seq(1:550)

x1 <- runif(500, 0, 90)
y1 <- runif(500, x1 + 10, 100)
x1.noise <- runif(50, 20, 80)
y1.noise <- 5/4 * (x1.noise - 10) + 0.1

x0 <- runif(500, 10, 100)
y0 <- runif(500, 0, x0 - 10)
x0.noise <- runif(50, 20, 80)
y0.noise <- 5/4 * (x0.noise - 10) - 0.1

x <- c(x1, x1.noise, x0, x0.noise)
y <- c(y1, y1.noise, y0, y0.noise)
z <- rep(0, 1100)
z[class1] <- 1
dat <- data.frame(x = x, y= y, z =as.factor(z))

plot(x[class1], y[class1], col = "red", pch = 2)
points(x[-class1], y[-class1], col = "blue", pch = 3)

#6b
tune.out <- tune(svm, z~., data = dat, kernel = "linear", 
                 ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000, 10000)))
summary(tune.out)

#7a
set.seed(1)
library(ISLR)

var <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto$mpglevel <- as.factor(var)

#7b
svm.auto <- tune(svm, mpglevel~.-mpg, data = Auto, kernel = "linear",
                 ranges = list(cost = c(.01, 0.1, 1, 5, 10, 100, 1000)))
summary(svm.auto)

#7c
set.seed(1)
svm.auto <- tune(svm, mpglevel~.-mpg, data = Auto, kernel = "radial", gamma = 1,
                 ranges = list(cost = c(.01, 0.1, 1, 5, 10, 100, 1000)))
summary(svm.auto)
svm.auto <- tune(svm, mpglevel~.-mpg, data = Auto, kernel = "polynomial", degree = 3, 
                 ranges = list(cost = c(.01, 0.1, 1, 5, 10, 100, 1000)))
summary(svm.auto)                 

#7d
svm.linear <- svm(mpglevel ~ ., data = Auto, kernel = "linear", cost = 1)
svm.poly <- svm(mpglevel ~ ., data = Auto, kernel = "polynomial", cost = 100, degree = 2)
svm.radial <- svm(mpglevel ~ ., data = Auto, kernel = "radial", cost = 100, gamma = 0.01)
plotpairs = function(fit) {
  for (name in names(Auto)[!(names(Auto) %in% c("mpg", "mpglevel", "name"))]) {
    plot(fit, Auto, as.formula(paste("mpg~", name, sep = "")))
  }
}
plotpairs(svm.linear)

#8a-b
train <- sample(1:nrow(OJ), 800)
test <- -train
svm.oj <- svm(Purchase~., data = OJ, kernel = "linear", cost = 0.01)
summary(svm.oj)
#8c
pred <- predict(svm.oj, OJ[train, ])
table(pred, OJ[train, "Purchase"]) 
(71 + 66)/800 #0.17125
pred <- predict(svm.oj, OJ[test, ])
table(pred, OJ[test, "Purchase"])
(11 + 29)/(11+29+143+87) #0.1481481
#8d
set.seed(1)
tune.out <- tune(svm, Purchase~., data = OJ, kernel = "linear",
                 ranges = list(cost = c(0.01, 0.1, 1, 10)))
summary(tune.out)
#8e
svm.oj <- svm(Purchase~., data = OJ, kernel = "linear", cost = 1)
pred <- predict(svm.oj, OJ[train, ])
table(pred, OJ[train, "Purchase"]) 
(69 + 64)/800 #0.166
pred <- predict(svm.oj, OJ[test, ])
table(pred, OJ[test, "Purchase"]) 
(10 + 29)/(144+29+10 +87) #0.144

#9e
svm.oj <- svm(Purchase~., data = OJ, kernel = "radial", gamma = 1, cost = 1)
pred <- predict(svm.oj, OJ[train, ])
table(pred, OJ[train, "Purchase"]) 
(39 + 53)/800 #0.115
pred <- predict(svm.oj, OJ[test, ])
table(pred, OJ[test, "Purchase"]) 
(6 + 29)/270 #0.1296

