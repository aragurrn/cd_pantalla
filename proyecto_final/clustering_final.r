#instalación de paquetes

library(plyr) 
library(ggplot2)
library(cluster)
library(lattice) 
library(graphics)
library(grid) 
library(scales)
library(gridExtra)
install.packages("readxl") #instalar paquete para leer excel
library("readxl") #instalar libreria para leer excel

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
