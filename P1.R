# Este es mi primer script
# Fecha: 10 al 24 de febrero 2021
# Autora: Ana E

#Hola
2+2 # Esto es una suma
2+2#Hola
2+2

2+5

5*3

#Para escribir comentarios y que no los lea como operaciones ponemos el símbolo de gato
# Lo podemos hacer para un comentario en una línea o la par de una instrucción
1:5               # Secuencia 1-5


seq(1, 10, 0.5)   # Secuencia con incrementos diferentes a 1

c('a','b','c')  # Vector con caracteres

list('a',1,'c')  # lista


40<80           # Valor logico

2+2 == 5        # Valor logico


T == TRUE       # T expresion corta de verdadero


#Objetos ####

x <- 24   
x

x/2

x <- TRUE       # Asigna el valor logico TRUE a la variable x OJO: x toma el ultimo valor que se le asigna

x/2

#Vectores ####
hola <- c(2,4,6)     # Vector numerico
y <- c('Primaria', 'Secundaria') # Vector caracteres

y[2]              # Acceder al segundo valor del vector y

y[3] <- 'Preparatoria y más' # Asigna valor a la tercera componente del vector


sex <-1:2         # Asigna a la variable sex los valores 1 y 2
names(sex) <- c("Femenino", "Masculino") # Asigna nombres al vector de elementos sexo
sex[2]


#Matrices ####
m <- matrix(nrow=2, ncol=3, 1:6, byrow = FALSE) # Matrices Ejemplo 1
m
View(m)


dim(m)
attributes(m)


m[1,1] <-7
m

n <- 1:6     # Matrices Ejemplo 2

dim(n) <- c(2,3)


xx <-10:12   # Matrices Ejemplo 3
yy<-14:16

cbind(xx,yy)
rbind(xx,yy)

mi_matrix<-cbind(xx,yy) # este resultado lo puedo asignar a un objeto

save.image("C:\\Users\\anaes\\Dropbox\\2021\\MEG\\repo-MEG\\P1.RData")

# Funciones -Aquí empieza sesion 6 ----

sum(10,20,30)    # Función suma


rep(6, times=3) # Repite la letra R el numero de veces que se indica

sqrt(9)           # Raiz cuadrada de 9


example(sum)

sum(1:5, NA, na.rm=TRUE) 


# Ambiente ----

ls() # lista todos los objetos en mi ambiente

rm(m)
rm(list=ls())


# Directorio ----

getwd()

setwd("C:/Users/anaes/Dropbox/2021/MEG/repo-MEG")

list.files()      # Lista de archivos en ese directorio


# Paquetes ----


install.packages("foreign", dependencies = TRUE)
install.packages("haven", dependencies = TRUE)

library(foreign)

tsdem<-read.dbf("./datos/TSDem.dbf") #checa cómo nos vamos adentro de nuestro directorio


# Instalar WDI

install.packages("WDI", dependencies = TRUE)
library(WDI)


WDIsearch('women')

busqueda<- WDI(country = "US",
    indicator = "SG.VAW.ARGU.ZS",
    start = 2015,
    end = 2020,
    extra = FALSE,
    cache = NULL)


WDI(country = "MX",
    indicator = "SG.VAW.1549.ZS",
    start = 1960,
    end = 2020,
    extra = FALSE,
    cache = NULL)


literacy<-WDI(country = "MX",
              indicator = "SE.ADT.1524.LT.FM.ZS",
              start = 1960,
              end = 2020,
              extra = FALSE,
              cache = NULL)

class(literacy)


plot(literacy$SE.ADT.1524.LT.FM.ZS)


plot(literacy$year, #primero pongo la x
     literacy$SE.ADT.1524.LT.FM.ZS) # luego pongo la y


plot(x=literacy$year,
     y=literacy$SE.ADT.1524.LT.FM.ZS) 
