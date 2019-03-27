source("load-libraries.R")

usarrests <- as_tibble(USArrests)
usarrests
names(usarrests) <- tolower(names(usarrests))
# apply(usarrests, 2, mean)
map_dbl(usarrests, mean)
map_dbl(usarrests, var)

# principal components analysis
pr.out <- prcomp(usarrests, scale = TRUE)
pr.out
summary(pr.out)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x) # score vectors: one column for each component

biplot(pr.out, scale = 0)

# reflect the image
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale = 0)

# std of each component
pr.out$sdev
# variance explained
pr.var <- pr.out$sdev^2
pr.var
# proportion of variance explained
pve <- pr.var/sum(pr.var)
pve

plot(pve, ylim = c(0, 1), type = 'b')
qplot(x = 1:4, y = pve, geom = "line", colour = I("dodgerblue2"))
plot(cumsum(pve), ylim = c(0, 1), type = 'b')
qplot(x = 1:4, y = cumsum(pve), geom = "line", colour = I("salmon"))

## K-Means Clustering

set.seed(2)
x = matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] = x[1:25, 1] + 3
x[1:25, 2] = x[1:25, 2] - 4
plot(x[1:25, 1], x[1:25, 2])

km.out <- kmeans(x, 2, nstart = 20)
km.out
summary(km.out)
km.out$cluster

plot(x, col = (km.out$cluster + 1), main = "K = 2", xlab = "", ylab = "", 
     pch = 20, cex = 2)

set.seed(4)
km.out <- kmeans(x, 3, nstart = 20)
km.out

set.seed(3)
km.out <- kmeans(x, 3, nstart = 1)
km.out$tot.withinss

km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss

## Hierarchical Clustering
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")

## plot dendograms
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete", xlab = "", sub = "", cex = 0.9)
plot(hc.average, main = "Average", xlab = "", sub = "", cex = 0.9)
plot(hc.single, main = "Single", xlab = "", sub = "", cex = 0.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

xsc <- scale(x)
plot(hclust(dist(xsc)))

# correlation-based distance measure
x <- matrix(rnorm(30 * 3), ncol = 3)
dd <- as.dist(1 - cor(t(x)))
plot(hclust(dd, method = "complete"), main = "Complete - Corr", 
     xlab = "", sub = "")

## Cancer data
rm(list = ls())
nci.labs <- NCI60$labs
nci.data <- NCI60$data

dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCR
pr.out <- prcomp(nci.data, scale = TRUE)

Cols <- function(vec){
    cols = rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
}

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z2")
plot(pr.out$x[, c(1,3)], col = Cols(nci.labs), pch = 19,
     xlab = "Z1", ylab = "Z3")

summary(pr.out)
plot(pr.out)

pve <- 100 * pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1, 2))
plot(pve, type = "o", ylab = "PVE", xlab = "Princ component",
     col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cum PVE", xlab = "Princ component",
     col = "brown3")

# hierarchical clustering
sd.data <- scale(nci.data)
par(mfrow = c(1, 1))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, 
     main = "Complete", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"), labels = nci.labs, 
     main = "Average", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "single"), labels = nci.labs, 
     main = "Single", xlab = "", sub = "", ylab = "")

hc.out <-  hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4) 
table(hc.clusters, nci.labs)

plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")

# K means clustering
set.seed(1)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)

# perform hierarchical clustering on first few component score vectors
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, main = "H Clus First Five Score Vec")

## Applied -------------
set.seed(1)
usarrests <- as_tibble(USArrests)
sld <- scale(usarrests)
ed <- dist(sld)^2
cd <- as.dist(1 - cor(t(sld)))

names(usarrests) <- tolower(names(usarrests))

dd <- as.dist(1 - cor(t(usarrests)))

rm(list = ls())
usarrests <- as_tibble(USArrests)
names(usarrests) <- tolower(names(usarrests))

pr.out <- prcomp(usarrests, scale = TRUE)
names(pr.out)
pr.var <- pr.out$sdev^2
pr.var
pve <- pr.var/sum(pr.var)
pve 

names(pr.out)
summary(pr.out)[[1]]

## 
rm(list = ls())
usarrests <- as_tibble(USArrests)
names(usarrests) <- tolower(names(usarrests))

scld.usarr <- scale(usarrests)
apply(scld.usarr, 2, mean)
apply(scld.usarr, 2, sd)

set.seed(2)

data.ed <- dist(usarrests)
hc.comp <- hclust(data.ed, method = "complete")
plot(hc.comp)
cutree(hc.comp, 3)

data.scld.ed <- dist(scld.usarr)
hc.scld.comp <- hclust(data.scld.ed, method = "complete")
plot(hc.scld.comp)
cutree(hc.scld.comp, 3)

table(cutree(hc.comp, 3), cutree(hc.scld.comp, 3))

## 10
set.seed(2)
x <- matrix(rnorm(20 * 3 * 50), ncol = 50)
x[1:20, 1:20] <- x[1:20, 1:20] + 5
x[21:40, 21:40] <- x[21:40, 21:40] - 3
x[41:60, 41:50] <- x[41:60, 41:50] - 8
xlab <- c(rep(1, 20), rep(2, 20), rep(3, 20))

# pcr
pr.out <- prcomp(x, scale = FALSE)
summary(pr.out)

par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = xlab, pch = 19,
     xlab = "Z1", ylab = "Z2")

# K-Means Clustering
set.seed(1)
km.out <- kmeans(x, 3, nstart = 20)
km.out$tot.withinss
km.clusters <- km.out$cluster
km.clusters
table(xlab, km.clusters)

set.seed(1)
km.out <- kmeans(x, 2, nstart = 20)
km.out$tot.withinss
km.clusters <- km.out$cluster
km.clusters
table(xlab, km.clusters)

set.seed(1)
km.out <- kmeans(x, 4, nstart = 20)
km.out$tot.withinss
km.clusters <- km.out$cluster
km.clusters
table(xlab, km.clusters)

# K-Means on first two principal components
km.out <- kmeans(pr.out$x[, 1:2], 3, nstart = 20)
km.out$tot.withinss
km.clusters <- km.out$cluster
km.clusters
table(xlab, km.clusters)

plot(pr.out$x[, 1:2], col = (km.out$cluster + 1), main = "K = 2", xlab = "", 
     ylab = "", pch = 20, cex = 2)

# with scaled data
set.seed(1)
km.out <- kmeans(scale(x), 3, nstart = 20)
km.out$tot.withinss
km.clusters <- km.out$cluster
km.clusters
table(xlab, km.clusters)

## 11
rm(list = ls())
gene.dat <- read_csv("./data/Ch10Ex11.csv", col_names = FALSE) 
dim(gene.dat) # data is transposed. predictors are rows; observations are cols
gene.dat

par(mfrow = c(1, 2))
set.seed(1)
dist.c <- as.dist(1 - cor((gene.dat)))
hclust.comp <- hclust(dist.c, method = "complete")
hclust.comp
summary(hclust.comp)
plot(hclust.comp)

hclust.avg <- hclust(dist.c, method = "average")
hclust.avg
summary(hclust.avg)
plot(hclust.avg)

# using pcr
pr.out <- prcomp(t(gene.dat))
loadings <- apply(pr.out$rotation, 1, sum)
