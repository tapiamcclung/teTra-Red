
# TALLER DE ANÁLISIS DE DEPENDENCIA Y AUTOCORRELACIÓN ESPACIAL. 
# UNA APROXIMACIÓN UTILIZANDO R 

# Nociones básicas de R

# Rodrigo Tapia-McClung  / tapia.mcclung@gmail.com
# Mario Martínez Salgado / mario.udir@gmail.com



## Contenido

# Preliminares
# Objetos en R
# Paquetes y librerías
# Importar datos
# Funciones
# Explorar una base de datos
# Subscripts
# Pegado de bases de datos
# Gráficos



#### PRELIMINARES ####

## Comentarios

# Todo aquello que se escriba a la derecha del signo numeral (#) se 
# coloreará de verde pálido y será tomado por R como un comentario.


## Ejecutar una instrucción: <Ctrl> + <R>

# Ubicar el cursor al inicio de la línea de comando o seleccionar un
# conjunto de líneas de comandos y oprimir las teclas <Ctrl> y <R>. 


## La Consola

# El signo '>' al final de la consola significa que R está listo para
# ejecutar la siguiente tarea.

# Un signo de '+' al final es indicativo de que la instrucción 
# ejecutada está incompleta.


## Operadores
  
# Aritméticos: +,  -,  *,  /  y  ^.
# Relacionales: >,  >=,  <,  <=,  ==  y  !=.
# Lógicos: & y |.
 
 

#### OBJETOS EN R ####
  
# Un objeto en R puede ser una tabla con datos, una base de datos, 
# una variable o un valor.

# Con el operador '<-' se asigna un valor a un objeto. Los objetos
# aparecen en la ventana superior de la derecha.
  

## Objetos numéricos
  
x <- 2

## Objetos de caracteres
  
aqui <- "ENES-UNAM"

## Vector numérico
  
cm <- c(167, 172, 153, 164, 182, 147)
kg <- c(48, NA, 55, 63, 71, 49)

## Vector de caracteres
  
nivel <- c("A", "B", "C", "D", "E", "F")
  

## Matrices

mv <- matrix(cm, nrow=3, ncol=2) 

mh <- matrix(cm, nrow=3, ncol=2, byrow=TRUE) 

  ## Llamar a los objetos
  
  mv
  
  mh


## Factor
  
# Objeto que almacena el valor de una variable categórica.
  
sexo <- factor(c("H", "M", "M", "M", "H", "M")) 
  
summary(sexo)  
  

## Data frame
  
# Un 'data frame' es más general que una matriz. Las columnas pueden
# tener diferentes clases de objetos (numéricos, factores, etc).
  
datos <- data.frame(nivel, sexo,cm, kg)
  
View(datos)


## Borrar objetos del workspace

rm(x, aqui) # Sólo algunos objetos

rm(list = ls()) # Todos los objetos



#### PAQUETES Y LIBRERÍAS ####
  
# En la Red existe un sin número de paquetes y están disponibles al
# público de manera gratuita. Para usar estos recursos hay que:
  
#   1o. Descargar e instalar el paquete de interés.
#   2o. Cargar el paquete a la sesión de trabajo.


# Ejemplo. Pirámide de población.

install.packages("pyramid")

library(pyramid)


# Población en localidades rurales. Michoacón, 2015.

hombres <- c(227088, 221051, 222669, 208826, 200237, 164498, 150676, 
             144043, 135905, 108809, 102534, 83350, 68458, 170317)

mujeres <- c(218558, 218376, 219155, 215099, 214932, 188959, 170648, 
             161999, 151192, 127456, 118636, 96193, 76053, 195867)

edad <-c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
         "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65 y +")

mich15 <- data.frame(hombres, mujeres, edad)

pyramid(mich15)


## Carpeta de trabajo

getwd()

# Cambiar carpeta de trabajo

setwd("C:/Users/marius/Desktop/DATOS/CURSOS")



#### IMPORTAR DATOS ####
  
# En la práctica es común encontrar/tener la información almacenada
# en varios formatos. Los más comunes son: dbf, csv, dta, sav y dat
  
# R puede cargar/abrir cualquier base de datos, no importa el 
# formato; sólo se necesita la librería 'foreign'.
  
install.packages("foreign")

library(foreign)
  

# Ejemplo: Cargar datos de un archivo de *.csv

mich10.csv <- read.csv("data/Distr edad MICH2010.csv")

enut <- read.dta("data/ENUT.dta")  


## Guardar una base de datos o una tabla en formato *.RData.

save(enut, file = "data/ENUT2014.RData")

rm(list=ls())


# Para cargar los datos utilizamos la función 'load()'.

load("data/ENUT2014.RData")



#### FUNCIONES ####

# Las funciones tienen nombre, argumentos y entregan un resultado 
# (valor, gráfico, archivo, ...).

  cm <- c(167, 172, 153, 164, 182, 147)
  kg <- c(48, NA, 55, 63, 71, 49)

mean(cm)

## Funciones con argumentos

sd(kg)

sd(kg, na.rm=TRUE)


## Crear una función

df <- read.dta("data/DFper.dta")  

piramide <- function(df){
  
  df$edad[df$edad>130] <- NA
  df$edad[df$edad>=100] <- 104
  
  m <- min(df$edad, na.rm=TRUE)
  M <- max(df$edad, na.rm=TRUE)
  
  df$g5 <- cut(df$edad, c(seq(from = (m-1), to = M, by=5)))
  
  aux <- data.frame(table(df$g5, df$sexo))
  aux2 <- data.frame(aux[1:(nrow(aux)/2),3],
                     aux[((nrow(aux)/2)+1):nrow(aux),3], 
                     aux[1:(nrow(aux)/2),1])
  
  pyramid (aux2, Llab="Hombres", Rlab="Mujeres", Clab="Edad", 
           Cstep=1, Cgap=0.15, AxisFM="fg", 
           AxisBM=",", Csize=1, Lcol="tomato", Rcol="cyan")
}

# Ejemplo. Distribución por edad y sexo de la ENUT

  enut <- read.dta("data/ENUT.dta")  

piramide(enut)



#### EXPLORAR UNA BASE DE DATOS ####
  
# ¿Qué variables tiene la ENUT?

names(enut)

# p 7.3: "En general, ¿qué tan feliz diría que es usted?"

# Para cambiar el nombre a una variable usamos la función 'rename'
# (se encuentra en el paquete 'reshape').

install.packages("reshape")

library(reshape)


## Renombrar la variable p7_3

enut <- rename(enut, c(p7_3  = "felicidad"))

names(enut)


## Selección de variables

# La forma de acceder a las variables en R es mediante el nombre del
# base (objeto), seguido del signo "$" y el nombre de la variable. 

# Desplegar los primeros valores de la variable 'edad'.

head(enut$edad)


## Crear una variable

# Tiempo dedicado a la limpieza del hogar

enut$limpiar <- enut$p6_5_2_2 + (enut$p6_5_2_3/60)


## Resumen de datos

## Tabla de frecuencias

# Distribución de los individuos según nivel de felicidad

table(enut$felicidad)

# 1 Nada; 2 Poco feliz; 3 Más o menos; 4 Feliz; y 5 Muy feliz


# Distribución incluyendo los valores perdidos

table(enut$felicidad, useNA = "always")


# Distribución de los individuos por 'felicidad' y 'sexo'

table(enut$felicidad, enut$sexo)


# Frecuencia relativa de los individuos por 'felicidad' y 'sexo'

# Por renglón (prop. hombres + prop. mujeres = 1)

prop.table(table(enut$felicidad, enut$sexo), 1)


# Por columna (prop. nada + ... + prop. muy feliz = 1)

prop.table(table(enut$felicidad, enut$sexo), 2)*100


## Función 'aggregate'

# Felicidad media por nivel de escolaridad (niv)

aggregate(enut$felicidad, by = list(enut$niv), 
          FUN = mean, na.rm = TRUE)


## Función 'summarySE'

  install.packages("Rmisc")
  library(Rmisc)

summarySE(enut, measurevar="limpiar", groupvars=c("sexo"), 
          na.rm = TRUE)



#### SUBSCRIPTS ####

# En ocasiones sólo se requiere aplicar las funciones a determinados
# elementos de un vector. Esto se consigue con los "subscripts". 

# Vector con las edades de las mujeres.

edad.m <- enut$edad[enut$sexo==2]


# Edad promedio de los hombres según el nivel de felicidad

aggregate(enut$edad[enut$sexo == 1], 
          by = list(enut$felicidad[enut$sexo == 1]), 
          FUN = mean, na.rm = TRUE)


# Cuando hay dos subscripts operando sobre un objeto, el 1o actúa 
# sobre los renglones y el segundo sobre las columnas. 

# El valor de una celda específica: registro 3, 7a variable (sexo).

enut[3,7] 


# Todos los valores del 3er individuo (todo el renglón).

enut[3, ] 


# Valores de ciertas variables del 10mo individuo.

enut[10,5:7] 


# El sexo de ciertas personas

enut[c(19,113,217), 7]


# Valores de una variable usando el nombre de la variable.

enut[c(5:9),"sexo"] 


## Selección de casos y/o variables

# Ejemplo 1. Seleccionar 5 variables de la ENUT

names(enut)

temp <- enut[ ,c("control","viv_sel", "hogar", "id_hog", "n_ren")]

head(temp)

temp.bis <- enut[enut$edad < 30,
                 c("control","viv_sel", "hogar", "id_hog", "n_ren")]


# También podemos utilizar la posición de la variable:

temp2 <- enut[ , c(1:5)]

head(temp2)


# Ejemplo 2. Crear una base con sólo la información de los hombres

enut_h <- enut[enut$sexo == 1, ]

head(enut_h)


# Ejemplo 2.1 Base con información* de las mujeres menores de 20.
#   * edad, parentesco (paren) y si asiste a la escuela (asiste_esc)

enut.m20 <- enut[enut$sexo == 2 & enut$edad<20, 
                 c("edad", "paren", "asiste_esc")]


# Ejemplo 3. Crear una base sólo con las variables identificador 
# y edad

enut2 <- subset(enut, select = c("control","viv_sel","hogar",
                                 "id_hog" ,"n_ren"  ,"edad"))

head(enut2)


# Ejemplo 4. Base de datos con la información de los mayores de 60, 
# pero sin las últimas 3 variables.

enut_60 <- subset(enut, edad>60, select=-c(299:301))

table(enut_60$edad, useNA="always")

names(enut_60)



#### PEGADO DE BASES DE DATOS ####

# Existen varias formas de pegar dos bases de datos. La más segura 
# es con la función 'merge()'.

# Es necesario tener una variable identificador (o un conjunto de  
# ellas). Esta variable permite articular las bases de interés.

names(enut)

base1 <- enut_60[,c("control", "viv_sel", "hogar", "id_hog", "n_ren",
                    "edad")]

base2 <- enut_h[,c("control", "viv_sel", "hogar", "id_hog", "n_ren",
                   "sexo")]

head(base1)

head(base2)


base3 <- merge(base1, base2, by=c("control", "viv_sel", "hogar", 
                                   "id_hog", "n_ren"), all=TRUE)  

# Con 'all=TRUE'se mantienen todos los casos.

head(base3)


# Con 'all=FALSE' se excluyen aquellos que no tienen las mismas 
# variables identificador.

base4 <- merge(base1, base2, by=c("control", "viv_sel", "hogar", 
                                   "id_hog", "n_ren"), all=FALSE)  

head(base4)


# Otra forma sería:

base5 <- merge(base1, base2, by=intersect(names(base1), names(base2)),
               all=TRUE)

rm(list = ls()) 



#### GRÁFICOS ####

load("data/ENUT2014.RData")
enut$limpiar <- enut$p6_5_2_2 + (enut$p6_5_2_3/60)


## De línea

# Ejemplo. Tiempo promedio dedicado a la limpieza del hogar por edad

limpieza <- aggregate(enut$limpiar, by = list(enut$edad),
                      FUN = mean, na.rm = TRUE)

head(limpieza)

names(limpieza) <- c("edad","media")
head(limpieza)

plot(limpieza$edad ,limpieza$media, type="l", xlab="Edad", 
     ylab="Tiempo promedio")


## Histogramas

# Ejemplo. Tiempo dedicado a cocinar

# Mujeres

hist(enut$p6_4_3_2[enut$sexo == 2], freq = FALSE, 
     ylab = "Frec. rel.", xlab = "Horas", breaks = 20, 
     ylim = c(0, 0.4), col = "purple")

# Hombres

hist(enut$p6_4_3_2[enut$sexo == 1], freq = FALSE, 
     ylab = "Frec. rel.", xlab = "Horas", breaks = 20, 
     ylim = c(0, 0.4), col = "cyan", add=TRUE)


## Gráfica de caja

boxplot(enut$limpiar ~ enut$sexo, 
        main = "Tiempo dedicado a limpiar")

  enut$sexof <- factor(enut$sexo, levels = c(1,2), 
                       labels = c("Hombres", "Mujeres"))

boxplot(enut$limpiar ~ enut$sexof, 
        main = "Tiempo dedicado a limpiar")


## Guardar en el escritorio las imágenes como un archivo *.png

getwd()
setwd("C:/Users/marius/Desktop")

png("Limpiar.png")
plot(limpieza$edad ,limpieza$media, type="l", xlab="Edad", 
     ylab="Tiempo promedio")
dev.off()


# Varias gráficas en una imagen

png("Arreglo de gráficas - 2 en 1.png", width = 700, height = 800)

par(mfrow = c(2,1))

boxplot(enut$escoacum ~ enut$p7_3, 
        main="Escolaridad por nivel de felicidad", 
        xlab="Nivel de felicidad", ylab="Años de escolaridad", 
        col="cyan")

plot(limpieza$edad ,limpieza$media, type="l", 
     main="Tiempo promedio dedicado a la \n limpieza del hogar por edad",
     xlab="Edad", ylab="Media de felicidad") 

par(mfrow = c(1,1))

dev.off()
