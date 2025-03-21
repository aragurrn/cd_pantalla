
# Using R to Perform a K-Means Analysis
#install.packages("gridExtra")

# install packages, if necessary
library(plyr) #split-apply-combine-paradigm
library(cluster)
library(ggplot2)
library(lattice) #gráficos multivariable
library(graphics)
library(grid) #gráficos en cuadrícula
#library(scales)

library(gridExtra)

# import the student grades
grade_input = as.data.frame(read.csv("C:/Users/hugoz/Desktop/cd_pantalla/cd_pantalla/p1/grades_km_input.csv"))

# transform into matrix for kmeans
kmdata_orig = as.matrix(grade_input[,c("Student","English", "Math","Science")])
kmdata <- kmdata_orig[,2:4]
#kmdata <- scale(kmdata,center=FALSE,scale=c(100,100,100))
#kmdata <- scale(kmdata,center=TRUE,scale=TRUE)
kmdata[1:10,]


#decide number of clusters
wss <- numeric(15) 
for (k in 1:15) wss[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares") 

km = kmeans(kmdata,3, nstart=25)
print(km)

c( wss[3] , sum(km$withinss) )

#prepare the data and clustering results for plotting
df = as.data.frame(kmdata)
df$cluster = factor(km$cluster)
centers=as.data.frame(km$centers)

g1 = ggplot(data=df, aes(x=English, y=Math, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=English,y=Math, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g2 = ggplot(data=df, aes(x=English, y=Science, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=English,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=Math, y=Science, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=Math,y=Science, color=as.factor(c(1,2,3))), 
             size=10, alpha=.3, show.legend=FALSE)

grid.arrange(arrangeGrob(grobs=list(g1 + theme(legend.position="none"),
                         g2 + theme(legend.position="none"),
                         g3 + theme(legend.position="none")),
                         main ="High School Student Cluster Analysis", ncol=1))




