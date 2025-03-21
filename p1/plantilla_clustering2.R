
# Using R to Perform a K-Means Analysis

# install packages, if necessary
library(plyr) #split-apply-combine-paradigm
library(ggplot2)
library(cluster)
library(lattice) #gráficos multivariable
library(graphics)
library(grid) #gráficos en cuadrícula
library(scales)
library(gridExtra)

#import the data
test1.input = as.data.frame(read.table("D:/owncloud/Data Science/clustering/s3.txt"))

#transform into matrix for kmeans
kmdata_orig = as.matrix(test1.input[,c("V1","V2")])
kmdata <- kmdata_orig[,1:2]
kmdata[1:10,]


#decide number of clusters
wss <- numeric(20) 
for (k in 1:20) wss[k] <- (sum(kmeans(kmdata, centers=k, nstart=20)$withinss))
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

lwss <- numeric(20)
for (k in 1:20) lwss[k] <- log(sum(kmeans(kmdata, centers=k, nstart=20)$withinss))
plot(1:20, lwss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

km1 = kmeans(kmdata,4, nstart=20)
km1

km2 = kmeans(kmdata,15, nstart=20)
km2


#prepare the data and clustering results for plotting
# 4 clusters

df = as.data.frame(kmdata)
df$cluster = factor(km1$cluster)
centers=as.data.frame(km1$centers)

g1= ggplot(data=df, aes(x=V1, y=V2, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=V1,y=V2, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

ggplot_build(g1) 



#15 clusters
df = as.data.frame(kmdata)
df$cluster = factor(km2$cluster)
centers=as.data.frame(km2$centers)

g2= ggplot(data=df, aes(x=V1, y=V2, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=V1,y=V2, color=as.factor(1:15)), 
             size=10, alpha=.3, show.legend=FALSE)

ggplot_build(g2) 



