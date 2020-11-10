fix(USArrests)
states <- row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
#    Murder    Assault   UrbanPop       Rape 
# 18.97047 6945.16571  209.51878   87.72916 

#PCA on USArrests
pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
pr.out$x
dim(pr.out$x)
pr.out$sdev #SD of each PC

biplot(pr.out, scale = 0, cex = 0.5)
pr.out$rotation = -pr.out$rotation
pr.out$x = -pr.out$x
biplot(pr.out, scale = 0, cex = 0.6)

#calculate  PVE
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve
plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = "b")
plot(cumsum(pve), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0, 1), type = "b")

#K-MEANS Clustering
#first create a data set that has two clusters
set.seed(2)
x <- matrix(rnorm(50*2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4 #first 25 observations move towards the right bottom corner

km.out <- kmeans(x, 2, nstart = 20)
km.out$cluster

plot(x, col = (km.out$cluster + 1), main = "K-Means Clustering Results with K=2", pch = 20, cex = 2)

#test different K numbers
set.seed(3)
km.out <- kmeans(x, 3, nstart = 20)
km.out$cluster
plot(x, col = (km.out$cluster + 1), pch = 20, cex = 2)

#test different start number nstart
set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss #104.3319
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss #97.9793

#Hierarchical Clustering
hc.complete <- hclust(dist(x), method = "complete")
hc.complete
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")

par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage", xlab = "", ylab = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", ylab = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", ylab = "", cex = .9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

xsc <- scale(x)
plot(hclust(dist(xsc), method = "complete"), main = "Hierarchical Clustering with Scaled Features")

x <- matrix(rnorm(30*3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete Linkage with Correlation-Based Distance", xlab = "", ylab = "")

#Lab3: NCI60 Data
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)
length(nci.labs)

table(nci.labs)

#PCA on the NCI60 Data
pr.out <- prcomp(nci.data, scale = TRUE)

Cols <- function(vec) {
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1, 3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")
summary(pr.out)

plot(pr.out)
pve <- 100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", xlab = "Principal Component", col = "Blue")

plot(summary(pr.out)$importance[2, ])
plot(summary(pr.out)$importance[3,])

#Clustering on NCI60 Data
sd.data <- scale(nci.data)
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)

plot(hclust(data.dist), labels = nci.labs, main = "Complete Linkage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, main = "Average Linkage", xlab = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, main = "Single Linkage", xlab = "", ylab = "")

hc.out <- hclust(dist(sd.data))
hc.cluster <- cutree(hc.out, 4)
table(hc.cluster, nci.labs)

par(mfrow = c(1, 1))
plot(hc.out, labels=nci.labs)
abline(h=139, col = "red")

hc.out

#Kmeans on NCI60
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.cluster)

hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "Hier.Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)

