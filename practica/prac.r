install.packages("dplyr")
library(dplyr)

setwd("C:/Users/hugoz/OneDrive/Desktop/cd_pantalla/cd_pantalla/practica")

whole_years <- as.data.frame(read.csv("C:/Users/hugoz/OneDrive/Desktop/cd_pantalla/cd_pantalla/practica/whole_years.csv",header=T,sep=",",dec="."))

whole_years2 <- filter(whole_years, year >= 2000 & year <= 2019)