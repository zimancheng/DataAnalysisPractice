auto <- read.table("~/R/ISL/Auto.data", header = T, na.strings = "?")
fix(auto)
attach(auto)

#8.a
lm.fit <- lm(mpg~horsepower, data = auto)
summary(lm.fit)
#i There is a relationship between mpg and horsepower.
#ii It is pretty strong, the R^2 is 0.6059 and the RSE is 4.906
#iii negative
#iv
predict(lm.fit, data.frame(horsepower = c(98)), interval = "confidence")
predict(lm.fit, data.frame(horsepower = 98), interval = "confidence")

#8b
plot(horsepower, mpg)
abline(lm.fit)

#8c
par(mfrow=c(2, 2))
plot(lm.fit)
#residuals vs fitted plot shows that the relationship is non-linear

#9a
data(Auto)
pairs(Auto)

#9b
cor(subset(auto, select=-name))

#9c
lm.fit2 <- lm(mpg~.-name, data = auto)
summary(lm.fit2)
#9ci yes
#9cii displacement, weight, year, origin
#9ciii it suggests that mpg has a positive relationship with year

#9d
par(mfrow=c(2, 2))
plot(lm.fit2)
#non-linearity & observation14 has high leverage

#9e
lm.fit3 <- lm(mpg~displacement + weight + year + origin, data = auto) 
lm.fit4 <- lm(mpg~displacement + weight + year * origin, data = auto)
lm.fit5 <- lm(mpg~displacement + year + weight * origin, data = auto)
lm.fit6 <- lm(mpg~year + weight + displacement * origin, data = auto)
summary(lm.fit3)
summary(lm.fit4)
summary(lm.fit5)
summary(lm.fit6)

#9f
lm.fit7 <- lm(mpg~poly(displacement, 3), data = auto)
summary(lm.fit7)
lm.fit8 <- lm(mpg~displacement + I(log(weight)), data = auto)
summary(lm.fit8)
lm.fit9 <- lm(mpg~displacement + I(weight^2), data = auto)
summary(lm.fit9)

#10a
library(ISLR)
data(Carseats)
lm.10fit <- lm(Sales~Price + Urban + US, data = Carseats)
summary(lm.10fit)
#10b
"Sales will drop by 54 for each dollar increase in Price (statistically significant)
Sales in urban are 22 lower than non-urban (not statistically significant)
Sales in US are 120 higher than non-US countries (statistically significant)
"
#10e
lm.10fit2 <- lm(Sales~Price + US, data = Carseats)
summary(lm.10fit2)

#10f
#compare R.sq and RSE

#10g
confint(lm.10fit2)

#10h 
par(mfrow=c(2,2))
# residuals v fitted plot doesn't show strong outliers
plot(lm.10fit2) 

par(mfrow=c(1,1))
# studentized residuals within -3 to 3 range
plot(predict(lm.10fit2), rstudent(lm.10fit2))

# load car packages
require(car)
# no evidence of outliers
qqPlot(lm.10fit2, main="QQ Plot")  # studentized resid

leveragePlots(lm.10fit2)  # leverage plots
plot(hatvalues(lm.10fit2))
# average obs leverage (p+1)/n = (2+1)/400 = 0.0075
# data may have some leverage issues


#11
#generate our data
rm(list = ls())
set.seed(1)
x = rnorm(100)
y = x * 2 + rnorm(100)

#11a
lm.fitY <- lm(y~x + 0)
summary(lm.fitY)

#11b
lm.fitX <- lm(x~y + 0)
summary(lm.fitX)


#12 b
rm(list = ls())
set.seed(1)
x = rnorm(100)
y = x * 2 + rnorm(100)
lm.fit1 <- lm(x~y)
lm.fit2 <- lm(y~x)
summary(lm.fit1)
summary(lm.fit2)

#12 c
rm(list = ls())
set.seed(1)
x <- rnorm(100, mean=1000, sd=0.1)
set.seed(1)
y <- rnorm(100, mean=1000, sd=0.1)

fit.lmY <- lm(y ~ x)
fit.lmX <- lm(x ~ y)
summary(fit.lmY)
summary(fit.lmX)

#13
rm(list = ls())
set.seed(1)
x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd = 0.25)
y <- -1 + 0.5*x + eps
plot(x, y)
lm.fit <- lm(y~x)
coef(lm.fit)

plot(x,y)
abline(-1, 0.5, col="blue")  # true regression
abline(lm.fit, col="red")    # fitted regression

lm.fit2 <- lm(y~x+ I(x^2))
summary(lm.fit2)
summary(lm.fit)


#15

library(MASS)
data("Boston")
names(Boston)
Boston$chas <- factor(Boston$chas, labels = c("N", "Y"))
names(Boston)[-1]
