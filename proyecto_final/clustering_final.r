#instalación de paquetes

library(plyr) 
install.packages("ggplot2")
library(ggplot2)
library(cluster)
library(lattice) 
library(graphics)
library(grid) 
library(scales)
install.packages("gridExtra")
library(gridExtra)
install.packages("readxl") #instalar paquete para leer excel
library("readxl") #instalar libreria para leer excel
install.packages("dplyr")

#fijar el wd
rep<-"C:/Users/hugoz/Desktop/cd_pantalla/cd_pantalla/proyecto_final"
setwd(rep)

#leer archivo excel y convertirlo a dataset
datos_conduccion<-as.data.frame(read_excel("datosconduccion.xlsx")) 
#aplicar correccion para asegurar que los datos son numéricos
datos_coonduccion_c<-datos_conduccion[sapply(datos_conduccion, is.numeric)]
#hay algunos valores NA que interfieren con el escalado
#esos valores se cambian por la mediana de su columna correspondiente para que el escalado no suponga un problema
datos_coonduccion_c<-as.data.frame(lapply(datos_coonduccion_c, function(col) {
  ifelse(is.na(col), median(col, na.rm = TRUE), col)
}))

#suma vectorial de las acelaeraciones
datos_coonduccion_c$Accel<-sqrt(datos_coonduccion_c$Accel.X^2 + datos_coonduccion_c$Accel.Y^2 + datos_coonduccion_c$Accel.Z^2)


#convertir a matriz para k-means
kmdata <- as.matrix(datos_coonduccion_c)


#escalado de datos
maximo<-apply(kmdata, 2, max, na.rm=TRUE)
#eliminar na, nan e inf
maximo[maximo == 0 | is.na(maximo)] <- 1
kmdata <- scale(kmdata,center=FALSE,scale=maximo)


#decidir el numero de clusters
wss <- numeric(16) 
for (k in 1:16) wss[k] <- (sum(kmeans(kmdata, centers=k, nstart=25)$withinss))
plot(1:16, wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")


km2 = kmeans(kmdata,2, nstart=25)
km2

km3 = kmeans(kmdata,3, nstart=25)
km3

km4 = kmeans(kmdata,4, nstart=25)
km4

km5 = kmeans(kmdata,5, nstart=25)
km5

#se concluye tras visualizar los datos procesados que se representa mejor el dataset con 4 clusters

#representación de datos
df = as.data.frame(kmdata)
df$cluster = factor(km4$cluster)
centers=as.data.frame(km4$centers)

g1= ggplot(data=df, aes(x=Accel, y=Speed, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=Accel, y=Speed, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g2 =ggplot(data=df, aes(x=Accel, y=RPM, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=Accel, y=RPM, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g3 = ggplot(data=df, aes(x=Accel, y=Throttle.Position, color=cluster )) + 
  geom_point() + theme(legend.position="right") +
  geom_point(data=centers, aes(x=Accel, y=Throttle.Position, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g4 = ggplot(data=df, aes(x=RPM, y=Gear, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=RPM, y=Gear, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g5= ggplot(data=df, aes(x=RPM, y=Throttle.Position, color=cluster )) + 
  geom_point() +
  geom_point(data=centers, aes(x=RPM, y=Throttle.Position, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g6 =ggplot(data=df, aes(x=Speed, y=Fuel.Trim, color=cluster )) + 
  geom_point() + 
  geom_point(data=centers, aes(x=Speed, y=Fuel.Trim, color=as.factor(1:4)), 
             size=10, alpha=.3, show.legend=FALSE)

g7= ggplot(data=df, aes(x=Engine.Load, y=Max.Speed, color=cluster )) + 
             geom_point() + 
             geom_point(data=centers, aes(x=Engine.Load, y=Max.Speed, color=as.factor(1:4)), 
                        size=10, alpha=.3, show.legend=FALSE)

grid.arrange(arrangeGrob(grobs=list(g1 + theme(legend.position="none"),
                                    g2 + theme(legend.position="none"),
                                    g3 + theme(legend.position="none"),
                                    g4 + theme(legend.position="none"),
                                    g5 + theme(legend.position="none"),
                                    g6 + theme(legend.position="none"),
                                    g7 + theme(legend.position="none")),
                         main ="Sign_data", ncol=3))

