#getwd()
setwd("D:/math-machinelearning")

2+3
5/9
log(5)
pi
exp(2)
sin(pi)
escalar <- 10
escalar
escalar2 <- 10
escalar + escalar2
escalar ^ 2
#numeric -> continuos
class(10)
class(1.5)

#integer -> discretos
class(5L)

#factor -> nominal / ordinal 
class('machine learning')

obj1 <- 10 
class(obj1)

obj1 <- as.character(obj1)
class(obj1)

is.character(obj1)

vec <- 10 
is.vector(vec)
vec1 <- c(2,4,6,8,9,10)
vec1

vec2 <- c("a", "b", "c", "d","e")
vec2

edades <- c(24,24,25,40,33,26)
class(edades)

edades[3]

hermanos <- c(2,2,2,3,4,3)
vec3 <- c(3:8)
vec3

seq(0,4, by=0.5)

seq(0,6, len=6)

edades + hermanos
edades * hermanos

#Reemplazar un elemento
vec3[1] <- 20

#Eliminar un objeto 
vec4 <- vec3[-1]

t(vec3)

matriz1 <- matrix(c(1:12), nrow=4, ncol=3, byrow = T)
matriz1

matriz2 <- matrix(c(1:12), nrow=3, ncol=4 ,byrow = T)
matriz2

#Multiplicacion de matrices
matriz1 %*% matriz2