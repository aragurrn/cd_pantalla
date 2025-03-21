x=2 #guarda copia en memoria
print(x)
x<-2 #no guarda en memoria,  es mejor usar este
print(x)
class(x) #tipo de dato
print(class(x))

x<-2.2
class(x)
print(class(x))

y<-"mipalo"
print(class(y))

z<-TRUE
print(class(z))
z<-21
print(class(z)) #se puede cambiar el tipo de dato

remove(x) #borra la variable x
#print(x) #error porque no existe

#variables predeclaradas
print(pi)
print(month.name)
print(month.name[1]) #los vectores empiezan en 1

#fechas
fecha<-"2020-01-01"
print(fecha)
fecha<-as.Date("01-01-2020")
print(fecha)
fecha<-as.Date("01/01/2020", format="%d/%m/%Y") #formato de fecha
print(fecha)

#horas
hora<-as.POSIXct("2020-01-01 12:00:00", format="%d/%m/%Y %H:%M:%OS") #fecha y hora formato POSIX (desde 1970)
print(hora)

#vectores
v<-c(1,2,3,4,5)
print(v)
v[3] #acceder a un elemento
print(v[3])
v[3]<-10 #modificar un elemento
print(v)
print(v[0]) #tipo del vector
print(v[1:3]) #rango de elementos extremos incluidos
print(v[-1]) #todos menos el primero
print(v[-c(1,3)]) #todos menos los elementos 1 y 3

vector_impares<-seq(1,100,2) #secuencia de 1 a 100 de 2 en 2
print(vector_impares)
vector_pares<-seq(1:50) #secuencia de 1 a 50
print(vector_pares)
vector_pares<-seq(1:50)*2 #secuencia de 1 a 50 multiplicado por 2
print(vector_pares)

#en los vectores solo se pueden almacenar tipos numéricos

#matrices
m1<-matrix(1:50, 10, 5) #matriz de 10x5 con los elementos de 1 a 50
print(m1) #rellena por columnas por defecto
m2<-matrix(1:50, 10, 5, byrow=TRUE) #rellena por filas
print(m2)
print(m2[1,3]) #acceder a un elemento, primera fila tercera columna
print(m2[1,]) #primera fila
print(m2[,3]) #tercera columna
print(m2[1:3,]) #primeras 3 filas
print(m2[,1:3]) #primeras 3 columnas
print(m2[1:3,1:3]) #primeras 3 filas y columnas
print(m2[-1,]) #todas menos la primera fila
print(m2[-c(1,3),]) #todas menos la primera y tercera fila
print(m2[-1,-3]) #todas menos la primera fila y tercera columna

#las matrices solo almacenan tipos numéricos

#listas
l1<-list(1, "hola", TRUE, c(1,2,3,4,5)) #lista con 4 elementos
print(l1)
print(l1[[1]]) #primer elemento
print(l1[[4]][2]) #segundo elemento del cuarto elemento

#dataframes
nombres<-c("Alice", "Bob", "Charlie")
edades<-c(23,19,45)
poblacion<-data.frame(nombres, edades) #dataframe con dos columnas
print(poblacion)
print(poblacion$nombres) #acceder a una columna
print(dim(poblacion)) #dimensiones
print(summary(poblacion)) #resumen de las columnas
print(str(poblacion)) #estructura del dataframe
print(head(poblacion)) #6 primeras filas

#poblacion$sexo<-c("F", "M", "M") #añadir columna al final
sexo<-c("F", "M", "M")
altura<-c(1.70, 1.80, 1.75)
poblacion<-cbind(poblacion[,c(1,2)], altura, sexo) #añadir columnas especificando el orden
print(poblacion) #así no se elimina la cabecera

poblacion$edades[2]<-20 #modificar un elemento
print(poblacion)

#subset
subpoblacion<-subset.data.frame(poblacion, select=c(nombres, altura)) #seleccionar columnas
print(subpoblacion)
subpoblacion2<-subset.data.frame(poblacion, edades>20) #filtro por valor
print(subpoblacion2)

poblacion$sangre<-c("A+", "B-", "?")
conval<-subset.data.frame(poblacion, sangre!="?") #filtro por condición
print(conval)

#estadística
print(range(poblacion$edades)) #rango
print(mean(poblacion$edades)) #media
print(sd(poblacion$edades)) #desviación estándar
print(var(poblacion$edades)) #varianza
print(table(poblacion$sexo)) #tabla de frecuencias
print(prop.table(table(poblacion$sexo))) #tabla de frecuencias relativas

#recta de regresión
poblacion$salario<-c(1000, 2000, 3000)
recta_regresion<-lm(poblacion$salario~poblacion$edades) #modelo de regresión lineal
print(recta_regresion)

#gráficos
plot(poblacion$edades, poblacion$salario) #gráfico de dispersión básico
plot(poblacion$edades, poblacion$salario, xlab="Edades", ylab="Salario", col=3, pch=16, cex=3,main="Pantalla")
#gráfico de dispersión editado
abline(2000, 0) #línea horizontal
abline(0, 100) #línea con pendiente
abline(recta_regresion, col=2) #recta de regresión

#leer archivos de datos
install.packages("readxl") #instalar paquete para leer excel
library("readxl") #instalar libreria para leer excel
#r puede leer csv por defecto

datos_excel<-as.data.frame(read_excel("medidas_cuerpo2.xlsx")) #leer archivo excel
print(datos_excel)

datos_csv_coma<-as.data.frame(read.csv("diabetes.csv")) #leer archivo csv con coma, caso fácil
print(datos_csv_coma)

datos_csv_dif<-as.data.frame(read.csv("medidas_cuerpo.csv", sep="\t", comment.char='#', header=TRUE)) #leer archivo csv caso dificil
print(datos_csv_dif)

#para rutas de ficheros
#rep<-"C:/Users/Usuario/Documents/medidas_cuerpo.csv"
#setwd(rep) #cambiar directorio de trabajo
#datos<-as.data.frame(read.csv("medidas_cuerpo.csv")) #leer archivo csv