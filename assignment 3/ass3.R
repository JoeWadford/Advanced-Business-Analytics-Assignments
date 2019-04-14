setwd("C:/Users/joewa/OneDrive/Documents/MBA/MBAD 6211/2- assignments/assignment 3")
library("data.table")
data <- fread("hospital_ortho.csv", sep=",", header=TRUE, strip.white=T, na.strings=c("NA","NaN","","?"))
data_NC <- data[(data$state=="NC")|(data$state=="SC")|(data$state=="TN")|(data$state=="VA")|(data$state=="GA")]

library("cluster")
library("clustertend")
library("dbscan")

data_NC$zip <- NULL
data_NC$hid <- NULL
data_NC$state <- NULL
data_NC$city <- NULL

if (TRUE){
  df <- scale(data_NC[-1]) #standardize the data
} else{
  df <- data_NC[-1]
}

k.means.fit <- kmeans(df, 2)
attributes(k.means.fit)

k.means.fit$centers

k.means.fit$cluster

k.means.fit$size

hopkins(df, n = nrow(df)-1)

withinsplot <- function(data_NC, nc=15, seed=1234){
  wss <- (nrow(data_NC)-1)*sum(apply(data_NC,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data_NC, centers = i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="within groups sum of squares")}
withinsplot(df, nc=10)

############################################################################

k.means.fit1 <- kmeans(df, 3)
attributes(k.means.fit1)

k.means.fit1$centers

k.means.fit1$cluster

k.means.fit1$size

hopkins(df, n = nrow(df)-1)

withinsplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data_NC)-1)*sum(apply(data_NC,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data_NC, centers = i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="within groups sum of squares")}
withinsplot(df, nc=10)

##################################################################################

clusplot(df, k.means.fit$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, 
         labels=2, lines=0)

data_NC$kmeans <- k.means.fit1$cluster  

d <- dist(df, method = "euclidean")
H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")
plot(H.complete)

H.average <- hclust(d, method = "average")
plot(H.average)

H.ward <- hclust(d, method = "ward.D2")
plot(H.ward)

groups <- cutree(H.ward, k=3)

plot(H.ward)
rect.hclust(H.ward, k = 3, border="red")


clusplot(df, groups, main="2D representation of the Cluster Solution",
         color = TRUE, shade = TRUE,
         labels = 2, lines = 0)

##############################################################################################

kNNdistplot(df, k = 4)
abline(h=4, col = "red")

db <- dbscan(df, eps = 4, minPts = 4)
db

clusplot(df, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

pca <- prcomp(data_NC[-1], center = TRUE, scale = TRUE)
summary(pca)

plot(pca, type = "lines")
library("devtools")

pca_data <- predict(pca, newdata = data_NC)
pc_df <- as.data.frame(scale(pca_data[,c(1:3)]))

k.means.fit.pc <- kmeans(pc_df, 3)
attributes(k.means.fit.pc)

k.means.fit.pc$centers

k.means.fit.pc$cluster

k.means.fit.pc$size

hopkins(pc_df, n = nrow(pc_df)-1)

withinsplot <- function(data_NC, nc=15, seed=1234){
  wss <- (nrow(data_NC)-1)*sum(apply(data_NC,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data_NC, centers = i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="within groups sum of squares")}
withinsplot(pc_df, nc=10)

####################################################################################

k.means.fit.pc1 <- kmeans(pc_df, 4)
k.means.fit.pc1$cluster

clusplot(pc_df, k.means.fit.pc1$cluster, main='2D representation of the Cluster solution', color=TRUE, shade=TRUE, 
         labels=2, lines=0)

d <- dist(pc_df, method = "euclidean")
H.single <- hclust(d, method = "single")
plot(H.single)

H.complete <- hclust(d, method = "complete")
plot(H.complete)

H.average <- hclust(d, method = "average")
plot(H.average)

H.ward <- hclust(d, method = "ward.D2")
plot(H.ward)

par(mfrow=c(1,1))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)

groups <- cutree(H.ward, k=3)

plot(H.ward)
rect.hclust(H.ward, k = 5, border="red")

############################################################################################

kNNdistplot(pc_df, k = 4)
abline(h=1, col = "red")

db <- dbscan(pc_df, eps = 1, minPts = 4)
db

clusplot(pc_df, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


pc_df$kmeans <- k.means.fit.pc1$cluster
pc_df$hclust <- groups
pc_df$db <- db$cluster
pc_df$hid <- data_NC$hid

final_data <- merge(x=pc_df, y=data_NC, key="hid")
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$kmeans), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$hclust), mean)
aggregate(final_data[,c("sales12","rbeds","hip12","knee12","femur12")], list(final_data$db), mean)
