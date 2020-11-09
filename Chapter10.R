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
