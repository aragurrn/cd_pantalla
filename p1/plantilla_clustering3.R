
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
install.packages("cluster.datasets")
library(cluster.datasets)
data(all.mammals.milk.1956)
str(all.mammals.milk.1956)
all.mammals.milk.1956$name

#transform into matrix for kmeans
kmdata <- as.matrix(all.mammals.milk.1956[,2:6])

maximo<-numeric(5)
for (k in 1:5) maximo[k]<-max(kmdata[,k])
kmdata <- scale(kmdata,center=FALSE,scale=maximo)

kmdata[1:10,]


#decide number of clusters
wss <- numeric(7) 
for (k in 1:7) wss[k] <- (sum(kmeans(kmdata, centers=k, nstart=25)$withinss))
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")


km2 = kmeans(kmdata,2, nstart=25)
km2

km3 = kmeans(kmdata,3, nstart=25)
km3

km4 = kmeans(kmdata,4, nstart=25)
km4


#prepare the data and clustering results for plotting
df = as.data.frame(kmdata)
df$cluster = factor(km4$cluster)
centers=as.data.frame(km4$centers)

g1= ggplot(data=df, aes(x=water, y=protein, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=water,y=protein, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g2 =ggplot(data=df, aes(x=water, y=fat, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=water,y=fat, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=water, y=lactose, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=water,y=lactose, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g4 = ggplot(data=df, aes(x=water, y=ash, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=water,y=ash, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)



g5= ggplot(data=df, aes(x=protein, y=fat, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=protein, y=fat, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g6 =ggplot(data=df, aes(x=protein, y=lactose, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=protein, y=lactose, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)


g7= ggplot(data=df, aes(x=protein, y=ash, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=protein, y=ash, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g8 =ggplot(data=df, aes(x=fat, y=lactose, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=fat, y=lactose, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g9 = ggplot(data=df, aes(x=fat, y=ash, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=fat,y=ash, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

grid.arrange(arrangeGrob(grobs=list(g1 + theme(legend.position="none"),
                                    g2 + theme(legend.position="none"),
                                    g3 + theme(legend.position="none"),
                                    g4 + theme(legend.position="none"),
                                    g5 + theme(legend.position="none"),
                                    g6 + theme(legend.position="none"),
                                    g7 + theme(legend.position="none"),
                                    g8 + theme(legend.position="none"),
                                    g9 + theme(legend.position="none")),
                         main ="Mammals milk analysis", ncol=3))
