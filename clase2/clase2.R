#Cargamos el conjunto de datos 
telco <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/Perdida%20de%20clientes.csv', sep = ';')

#Observamos el conjunto de datos
#Variable dependiente -> desafiliado
head(telco)

empresa <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/publicidad.csv', sep = ';')

#Observamos el conjunto de datos
#Variable dependiente -> Ventas
head(empresa)

#observamos el tipo de datos
str(telco)

#continua num -> histogramas histograma
#texto factor -> 
# tipo int(discreto) - > pocos valores / barras, muchos valores (grafico de histograma) 
#variable dependiente siempre en factor

#Resumen de los datos
summary(telco)

library(Hmisc)
describe(telco)


#Entendimiento de los datos
target <- table(telco$Desafiliado)
barplot(target)

names(telco)
#Graficamos lo mismo con la librería ggplot2
library(ggplot2)
ggplot(data=telco)+ 
  geom_bar(mapping =  aes(x=Desafiliado, color='Blue'))

ggplot(data=telco)+ 
  geom_bar(mapping =  aes(x=Plan_Internacional, fill=Desafiliado))

ggplot(data=telco)+ 
  geom_histogram(mapping =  aes(x=Min_En_Dia, fill=Desafiliado))

ggplot(data=telco)+ 
  geom_bar(mapping =  aes(x=Reclamos, fill=Desafiliado))

ggplot(data=telco)+ 
  geom_bar(mapping =  aes(x=Llamadas_Internacionales, fill=Desafiliado))


ggplot(data=telco)+ 
  geom_point(mapping =  aes(x=Reclamos, y=Min_En_Dia, color=Desafiliado))

data(iris)
head(iris)

ggplot(data=iris)+ 
  geom_histogram(mapping =  aes(x=Sepal.Length, fill=Species))

ggplot(data=iris)+ 
  geom_histogram(mapping =  aes(x=Sepal.Width, fill=Species))

ggplot(data=iris)+ 
  geom_histogram(mapping =  aes(x=Petal.Length, fill=Species))

ggplot(data=iris)+ 
  geom_histogram(mapping =  aes(x=Petal.Width, fill=Species))

ggplot(data=iris)+ 
  geom_point(mapping =  aes(x=Petal.Length, y=Petal.Width, color=Species))

ggplot(data=iris)+ 
  geom_histogram(mapping =  aes(x=Sepal.Width, fill=Species))

library(scatterplot3d)
scatterplot3d(iris[,1:3], color= as.integer(iris$Species))

?scatterplot3d

library(rgl)
gif <- plot3d(iris[,1:3], col= as.integer(iris$Species), type = 's', radius = 0.1)
play3d(spin3d(gif), duration = 20)

gif1 <- plot3d(telco[,2:4], col= as.integer(telco$Desafiliado), type = 's', radius = 0.9)
play3d(spin3d(gif1), duration = 1)

#Se trata de evaluar relaciones 
plot(telco)

plot(empresa)

library(aplpack)
#Se utiliza mas en análisis no supervisado(clusters)
faces(telco[1:20,2:5])

#Covarianza variables cuantitativas
cov(empresa)

cor(empresa)

library(corrplot)
corrplot(cor(empresa), method = c('square'))
corrplot(cor(empresa), method = c('number'))

library(PerformanceAnalytics)
chart.Correlation(iris[,1:4])

boxplot(empresa$Ventas, col=3)
boxplot(empresa$Periodico, col=3)

hist(empresa$Periodico, col = 'red')

par(mfrow=c(1,2))
boxplot(empresa$Periodico, col=3)
hist(empresa$Periodico,col=2)
par(mfrow=c(1,1))

plot(density(empresa$Periodico))
library(fBasics)
skewness(empresa$Periodico)
kurtosis(empresa$Periodico)


modelo1 <- lm(Ventas~Television, data=empresa)
summary(modelo1)

modelo2 <- lm(Ventas~Television+Radio, data=empresa)
summary(modelo2)

modelo3 <- lm(Ventas~Television+Radio+Periodico, data=empresa)
summary(modelo3)




