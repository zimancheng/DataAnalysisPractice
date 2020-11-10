#3
x <- cbind(c(1, 1, 0, 5, 6, 4), c(4, 3, 4, 1, 2, 0))
plot(x[,1], x[,2])
set.seed(1)
labels <- sample(c(0, 1), 6, rep = T)
plot(x[, 1], x[, 2], col = labels + 1, pch = 20)

centroid1 <- c(mean(x[labels == 0, 1]), mean(x[labels == 0, 2]))
centroid2 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
points(centroid1[1], centroid1[2], col = 1, pch = 10)
points(centroid2[1], centroid2[2], col = 2, pch = 10)

labels <- c(1, 1, 1, 0, 0, 0)
plot(x[, 1], x[, 2], col = labels + 1, pch = 20)
centroid1 <- c(mean(x[labels == 0, 1]), mean(x[labels == 0, 2]))
centroid2 <- c(mean(x[labels == 1, 1]), mean(x[labels == 1, 2]))
points(centroid1[1], centroid1[2], col = 1, pch = 10)
points(centroid2[1], centroid2[2], col = 2, pch = 10)
#stopped changing clusters

#7
library(ISLR)
set.seed(1)
dsc <- scale(USArrests)
d1 <- dist(dsc)^2
d2 <- as.dist(1 - cor(t(dsc)))
summary(d2 / d1)

#8
pr.out <- prcomp(USArrests, scale = T)
pve <- pr.out$sdev^2/sum(pr.out$sdev^2)
loadings <- pr.out$rotation
sumvar <- sum(apply(dsc^2, 2, sum))
apply((dsc %*% loadings)^2, 2, sum) / sumvar

#9
set.seed(2)
hc.us <- hclust(dist(USArrests), method = "complete")
plot(hc.us)
cutree(hc.us, 3)

sd.data <- scale(USArrests)
hc.sd <- hclust(dist(sd.data), method = "complete")
plot(hc.sd)
cutree(hc.sd, 3)

table(cutree(hc.sd, 3), cutree(hc.us, 3))
#Scaling the variables affect the clusters obtained although the trees are somewhat similar. The variables should be scaled beforehand because the data measures have different units.

#10
set.seed(1)
x <- matrix(rnorm(60*50), ncol = 50)
x[1:20, 2] <- 1
x[21:40, 1] <- 2
x[21:40, 2] <- 2
x[41:60, 1] <- 1
labels <- c(rep(1, 20), rep(2, 20), rep(3, 20))

pr.out <- prcomp(x)
plot(pr.out$x[, 1:2], col = labels, xlab = "Z1", ylab = "Z2")

#10c
km.out <- kmeans(x, 3, nstart = 20)upy
km.out$cluster
table(km.out$cluster, labels)

#11
