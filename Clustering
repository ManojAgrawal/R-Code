library(dplyr)
library(cluster)
library(Hmisc)
library(tidyr)

# copy paste data from clipboard to R dataframes
transactions <- read.delim("clipboard")

offers <- read.delim("clipboard")

# merge and prepare data
transactions <- merge(transactions,offers,by.x="Offer",by.y="Offer")

transactions$qty <- 1

CustomerOffers <- transactions %>%
  group_by(Customer, Campaign, Varietal, Discount, Origin, Past.Peak) %>%
  spread(Customer, qty)

CustomerOffers[is.na(CustomerOffers)] <- 0

# drop first 6 columns
CustomerDat <- CustomerOffers[,-(1:7)]
CustomerDat.t <- t(CustomerDat)

print(CustomerDat.t)

# Setup for k-means loop 
km.out <- list()
sil.out <- list()
x <- vector()
y <- vector()
minClust <- 4      # Hypothesized minimum number of segments
maxClust <- 8      # Hypothesized maximum number of segments


# Compute k-means clustering over various clusters, k, from minClust to maxClust
for (centr in minClust:maxClust) {
  i <- centr-(minClust-1) # relevels start as 1, and increases with centr
  set.seed(10) # For reproducibility
  km.out[i] <- list(kmeans(CustomerDat.t, centers = centr, nstart = 25))
  sil.out[i] <- list(silhouette(km.out[[i]][[1]], dist(CustomerDat.t)))
  # Used for plotting silhouette average widths
  x[i] = centr  # value of k
  y[i] = summary(sil.out[[i]])[[4]]  # Silhouette average width
}

# Plot silhouette results to find best number of clusters; closer to 1 is better
library(ggplot2)
ggplot(data = data.frame(x, y), aes(x, y)) + 
  geom_point(size=3) + 
  geom_line() +
  xlab("Number of Cluster Centers") +
  ylab("Silhouette Average Width") +
  ggtitle("Silhouette Average Width as Cluster Center Varies")

# Get customer names that are in each segment ----------------------------------

# Get attributes of optimal k-means output
maxSilRow <- which.max(y)          # Row number of max silhouette value
optimalClusters <- x[maxSilRow]    # Number of clusters
km.out.best <- km.out[[maxSilRow]] # k-means output of best cluster

# Create list of customer names for each cluster
clusterNames <- list()
clusterList <- list()
for (clustr in 1:optimalClusters) {
  clusterNames[clustr] <- paste0("X", clustr)
  clusterList[clustr] <- list(
    names(
      km.out.best$cluster[km.out.best$cluster == clustr]
    )
  )
}
names(clusterList) <- clusterNames

print(clusterList)

# Combine cluster centroids with bike models for feature inspection ------------
custSegmentCntrs <- t(km.out.best$centers)  # Get centroids for groups
colnames(custSegmentCntrs) <- make.names(colnames(custSegmentCntrs))
customerOffers.clustered <- cbind(as.data.frame(CustomerOffers[,1:7]), custSegmentCntrs)

# Arrange top 10 bike models by cluster in descending order --------------------
attach(customerOffers.clustered)  # Allows ordering by column name
knitr::kable(head(customerOffers.clustered[order(-X1), c(1:5, 6)], 10))

knitr::kable(head(customerOffers.clustered[order(-X2), c(1:5, 7)], 10))

knitr::kable(head(customerOffers.clustered[order(-X3), c(1:5, 8)], 10))

knitr::kable(head(customerOffers.clustered[order(-X4), c(1:5, 9)], 10))
