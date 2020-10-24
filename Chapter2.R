x <- c(1, 3, 2, 5)
x
x = c(1, 6, 3)
y = c(1, 4, 3)
length(x)
x+y

ls() # show a list of all the objects, including data and functions

rm(x, y) #delete any unwanted objects
ls()
rm(list = ls()) # remove all objects at once

?matrix # opens the help page of matrix
x = matrix(data = c(1, 2, 3, 4), nrow = 2, ncol = 2)
x
x = matrix(c(1, 2, 3, 4), 2, 2, byrow = TRUE)
# byrow is default FALSE, which leads to 1324, change it to TRUE can get 1234
x

sqrt(x)
?sqrt

x ^ 2

?rnorm #rnorm() generates a vector of random normal variables, in normal distribution

x = rnorm(50)
y = x + rnorm(50, mean = 50, sd = .1)
cor(x, y)
set.seed(1303)
rnorm(50)
set.seed(3)
y = rnorm(100)
mean(y)
var(y)
sqrt(var(y)) # standard deviation
sd(y) # standard deviation

"this is an example of 
multiple lines of comments
"

"
2.3.2 Graphics
"

x = rnorm(100)
y = rnorm(100)
plot(x, y)
plot(x, y, xlab="this is the x-axis", ylab = "this is the y-axis",
     main = "Plot of X vs Y")
pdf("figure.pdf")
plot(x, y)
dev.off()

?plot

getwd()
setwd("C:/Users/ziman/Documents/R")
getwd()

x = seq(1, 10)
x
x = 1:10
x
x = seq(-pi, pi, length=50)
x
y = x
f = outer(x, y, function(x, y)cos(y)/(1 + x^2))
contour(x, y, f)
contour(x, y, f, nlevels = 45, add=T)
g = t(f)
?t() # returns the transpose of x
fa = (f - g)/2
contour(x, y, fa, nlevels=15)
image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa, theta = 30)
persp(x, y, fa, theta = 30, phi = 20)
persp(x, y, fa, theta = 30, phi = 70)

"
2.3.3 Indexing Data
"

A = matrix(1:16, 4, 4)
A[2, 3] # notice the indexing in R starts in 1, instead of 0 as in other programming languages
A[c(1, 3), c(2, 4)] # row 1& 3, col 2 & 4
A[1:3, 2:4]
A[, 2:4]
A[1:3, ]
A[-c(1, 3), ] # keep all rows or columns except those indicated in the index

dim(A)

"
2.3.4 Loading Data
"
Auto=read.table("~/R/ISL/Auto.data")
fix(Auto) # used to view it in a spreadsheet like window
Auto=read.table("~/R/ISL/Auto.data", header=T, na.strings = "?")
fix(Auto)
dim(Auto)
Auto = na.omit(Auto) # remote empty rows
dim(Auto)

names(Auto) # returns variable names

plot(cylinders, mpg)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)
plot(cylinders, mpg)

#convert quantitative variables into qualitative variables
# then it's box plots 
cylinders = as.factor(cylinders)
plot(cylinders, mpg)
plot(cylinders, mpg, col="red")
plot(cylinders, mpg, col = "blue", varwidth=T)
plot(cylinders, mpg, col = 'yellow', varwidth = T, horizontal= T)
plot(cylinders, mpg, col = 'Green', varwidth=T, xlab='cylinders', ylab = 'MPG')

# histogram


hist(mpg)
hist(mpg, col=2)
hist(mpg, col='green', breaks = 15)
hist(mpg, col='green', breaks = 30)

# scatterplots

pairs(~ mpg + displacement + horsepower + weight + acceleration, Auto)

# interactive function
plot(horsepower, mpg)
identify(horsepower, mpg, name)

# summary
summary(Auto)

summary(mpg)
q()
