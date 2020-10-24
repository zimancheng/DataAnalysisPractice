college = read.csv("~/R/ISL/College.csv", stringsAsFactors = T)
fix(college)
rownames(college) = college[,1]
fix(college)
college = college[, -1]
fix(college)

# 8(c)
summary(college)
pairs(college[, 1:10])
attach(college)
Private = as.factor(Private)
plot(Private, Outstate)

Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Elite, Outstate, xlab = "Elite", ylab= "Outstate")

#histogram
par(mfrow=c(2, 2))
hist(Outstate)
hist(Books, breaks = 5)
hist(Grad.Rate, breaks = 5)

#college which has the highest acceptance rate
college[which.max(Enroll/Accept), ]
# here we treat college as a table, or more specifically data.frame. you can use slicing

# 9
Auto = read.table("~/R/ISL/Auto.data", header=T, na.strings = "?")
Auto <- na.omit(Auto)

summary(Auto)

#9(b)
qualitative_var <- c(2, 8, 9)
sapply(Auto[, -qualitative_var], range)

#9(c)
sapply(Auto[, -qualitative_var], mean)
sapply(Auto[, -qualitative_var], sd)

#9(d)
sapply(Auto[-seq(10, 85), -qualitative_var], mean)
sapply(Auto[-seq(10, 85), -qualitative_var], range)
sapply(Auto[-seq(10, 85), -qualitative_var],sd)

#9(e)
pairs(Auto[, -qualitative_var])
plot(year, mpg)
dev.off()

#9(f)
plot(as.factor(cylinders), mpg, xlab = "cylinders", ylab = "mpg", col = "green")
plot(horsepower, mpg)


# Exercise 10
library(MASS)
Boston
?Boston

#10(b)
all_correlations <- cor(Boston)
print(all_correlations[, 14])
cols <- c(14, 13, 6, 11, 3, 10)
pairs(Boston[, cols])
dev.off()

#10(c)
dim(Boston)
plot(Boston$dis, Boston$crim)
plot(Boston[, 1], Boston[, 9])

#10(d)
require(ggplot2)
require(reshape2)
# plot each feature against crim rate
bosmelt <- melt(Boston, id="crim")
ggplot(bosmelt, aes(x=value, y=crim)) +
  facet_wrap(~variable, scales="free") + 
  geom_point()
(corrmatrix <- cor(Boston, use="complete.obs")[1,])
corrmatrix[corrmatrix > 0.5 | corrmatrix < -0.5][-1]


#10(d)
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=crim))
g + geom_point()
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=tax))
g + geom_point()
g <- ggplot(Boston, aes(x=1:nrow(Boston), y=ptratio))
g + geom_point()

#10(e)
table(Boston$chas)
# 35 towns near the Charles river

#10(f)
median(Boston$ptratio)
#19.05

#10(g)
seltown <- Boston[Boston$medv == min(Boston$medv), ]
seltown

sapply(Boston, quantile)

#10(h)
nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])
rbind(sapply(Boston[Boston$rm > 7, ], median), sapply(Boston, median))
