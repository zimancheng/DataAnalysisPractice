library(ISLR)
fix(Wage)
dim(Wage)
attach(Wage)

#Polynomial Regressions
fit <- lm(wage~poly(age, 4), data = Wage)
summary(fit)
coef(summary(fit))

fit <- lm(wage~poly(age, 4, raw = T), data = Wage)
coef(summary(fit))

fit2 <- lm(wage~cbind(age, age^2, age^3, age^4), data = Wage)
coef(summary(fit2))

#generate prediction and standard errors
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
#age.grid2 <- seq(agelims[1], agelims[2])
pred <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(pred$fit + 2 * pred$se.fit, pred$fit - 2 * pred$se.fit)

#plot the prediction and standard errors
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1), oma = c(0, 0, 4, 0))
plot(age, wage, xlab = "Age", ylab = "Wage", xlim = agelims, cex = .5, col = "darkgrey")
title("Degree-4 Polynomial", outer = T)
lines(age.grid, pred$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

pred2 <- predict(fit2, newdata = list(age = age.grid), se = T)
max(abs(pred$fit - pred2$fit))

fit.1 <- lm(wage~age, data = Wage)
fit.2 <- lm(wage~poly(age, 2), data = Wage)
fit.3 <- lm(wage~poly(age, 3), data = Wage)
fit.4 <- lm(wage~poly(age, 4), data = Wage)
fit.5 <- lm(wage~poly(age, 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

coef(summary(fit.5))

#Polynomial regression for classification problems
fit <- glm(I(wage>250)~poly(age, 4), data = Wage, family = binomial)
pred <- predict(fit, newdata = list(age = age.grid), se = T)
pfit <- exp(pred$fit)/(1 + exp(pred$fit))
se.bands.logit <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
se.bands <- exp(se.bands.logit)/(1 + exp(se.bands.logit))

#pred <- predict(fit, newdata = list(age = age.grid), type = "response") 
#returns probability directly

plot(age, I(wage>250), xlim = agelims, type = "n", ylim = c(0, 0.2))
points(jitter(age), I((wage>250)/5), cex = .5, pch = "|", col = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)

dev.off()
#Step Functions
table(cut(age, 4))
fit <- lm(wage~cut(age, 4), data = Wage)
coef(summary(fit))

#prediction
pred <- predict(fit, newdata = list(age = age.grid), se = T)
se.bands <- cbind(pred$fit + 2 * pred$se.fit, pred$fit - 2 * pred$se.fit)
par(mfrow = c(1, 1))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Step Function")
lines(age.grid, pred$fit, lwd = 2, col = "blue")
matlines(age.grid, se.bands, lwd = 1, col = "blue", lty = 3)
