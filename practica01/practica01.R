library(Hmisc)
library(ggplot2)
library(scatterplot3d)
library(rgl)
library(aplpack)
library(corrplot)
library(PerformanceAnalytics)
library(fBasics)
library(gmodels)
library(rpart.plot)
library(partykit)
library(caret)
library(e1071)
library(mlr)

train<-read.csv("bostonvivienda.csv",sep = ';') # leer la data de entrenamiento
head(train)

names(train)
str(train)

summary(train)

#Examinamos la variable crimen per c'apita
hist(train$crim, breaks = 100, main = "Crimen per capita",xlab = "Crimen",col="blue")

#Proporción de terrenos residenciales divididos en zonas para lotes de más de 25,000 pies cuadrados
hist(train$zn, breaks = 100, main = "Terrenos residenciales",xlab = "Zn",col="blue")

#Proporción de acres de negocios no minoristas por ciudad
hist(train$indus, breaks = 100, main = "Negocios no minoristas",xlab = "Zn",col="blue")

#Si limita con el rio =1, si no 0
hist(train$chas, breaks = 100, main = "LImite con el rio",xlab = "Zn",col="blue")

#concentración de óxidos nítrico
hist(train$nox, breaks = 100, main = "concentración de óxidos nítrico",xlab = "Zn",col="blue")

#número promedio de habitaciones por vivienda
hist(train$rm, breaks = 100, main = "Número promedio de habitaciones por vivienda",xlab = "Zn",col="blue")

#Proporción de unidades ocupadas por sus propietarios construidas antes de 1940
hist(train$edad, breaks = 100, main = "Proporción de unidades ocupadas por sus propietarios construidas antes de 1940",xlab = "Zn",col="blue")

#Distancias desproporcionadas a cinco centros de empleo de Boston
hist(train$dis, breaks = 100, main = "Distancias desproporcionadas a cinco centros de empleo de Boston",xlab = "Zn",col="blue")


#índice de accesibilidad a las autopistas radiales
hist(train$rad, breaks = 100, main = "índice de accesibilidad a las autopistas radiales",xlab = "Zn",col="blue")

#tasa de impuesto a la propiedad de valor completo por USD 10,000
hist(train$impuesto, breaks = 100, main = "tasa de impuesto a la propiedad de valor completo por USD 10,000s",xlab = "Zn",col="blue")

#colegios por localidad
hist(train$ptratio, breaks = 100, main = "colegios por localidad",xlab = "Zn",col="blue")

#es la proporción de negros por ciudad
hist(train$negro, breaks = 100, main = "Es la proporción de negros por ciudad",xlab = "Zn",col="blue")

#porcentaje de estado inferior de la población
hist(train$lstat, breaks = 100, main = "porcentaje de estado inferior de la población",xlab = "Zn",col="blue")

#valor medio de las viviendas ocupadas por sus propietarios en USD 1000
hist(train$medv, breaks = 100, main = "valor medio de las viviendas ocupadas por sus propietarios en USD 1000",xlab = "Zn",col="blue")

#Variables que parecen tener outliers
boxplot(train$crim, col=3)
boxplot(train$zn, col=3)
boxplot(train$edad, col=3)
boxplot(train$negro, col=3)
boxplot(train$medv, col=3)

#crim
qnt <- quantile(train$crim, probs=c(.25, .75), na.rm = T)
caps <- quantile(train$crim, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(train$crim, na.rm = T)
train$crim[train$crim < (qnt[1] - H)] <- caps[1]
train$crim[train$crim > (qnt[2] + H)] <- caps[2]

#zn
qnt <- quantile(train$zn, probs=c(.25, .75), na.rm = T)
caps <- quantile(train$zn, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(train$zn, na.rm = T)
train$zn[train$zn < (qnt[1] - H)] <- caps[1]
train$zn[train$zn > (qnt[2] + H)] <- caps[2]

#edad
qnt <- quantile(train$edad, probs=c(.25, .75), na.rm = T)
caps <- quantile(train$edad, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(train$edad, na.rm = T)
train$edad[train$edad < (qnt[1] - H)] <- caps[1]
train$edad[train$edad > (qnt[2] + H)] <- caps[2]

#negro
qnt <- quantile(train$negro, probs=c(.25, .75), na.rm = T)
caps <- quantile(train$negro, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(train$negro, na.rm = T)
train$negro[train$negro < (qnt[1] - H)] <- caps[1]
train$negro[train$negro > (qnt[2] + H)] <- caps[2]

target <- table(train$medv)
barplot(target)

ggplot(data=train)+ 
  geom_bar(mapping =  aes(x=medv, color='Blue'))

ggplot(data=train)+ 
  geom_bar(mapping =  aes(x=ptratio, color='Blue'))

ggplot(data=train)+ 
  geom_histogram(mapping =  aes(x=impuesto, fill=medv))

plot(train)
cov(train)
cor(train)

corrplot(cor(train), method = c('square'))
corrplot(cor(train), method = c('number'))

boxplot(train$edad, col=3)
boxplot(train$chas, col=3)

plot(density(train$impuesto))

resumen=data.frame(summarizeColumns(train))

perdidos=data.frame(resumen$name,resumen$na,resumen$type); colnames(perdidos)=c("name","na","type")

perdidos <- resumen %>% select(name, na, type)
perdidos

train_parametrica <- impute(train, classes = list(integer = imputeMode(),
                                                  numeric = imputeMean()),
                            dummy.classes = c("integer","numeric"), dummy.type = "numeric")
train_parametrica=train_parametrica$data[,1:min(dim(train))]
head(train_parametrica)

#Simple linear 
library(caTools)
#set.seed(123)
#split = sample.split(train$medv, SplitRatio= 2/3)
#training_set = subset(train, split==TRUE)
#test_set = subset(train, split==TRUE)

set.seed(123)
split = sample.split(train_parametrica$medv, SplitRatio= 2/3)
training_set = subset(train_parametrica, split==TRUE)
test_set = subset(train_parametrica, split==TRUE)

modeloreg <- lm(medv~.,data = training_set)
modeloreg
summary(modeloreg)

y_pred = predict(modeloreg, newdata = test_set)
table(y_pred)

#Backward Elimination
modeloreg <- lm(medv ~ crim+zn+chas+nox+rm+dis+rad+impuesto+ptratio+negro+lstat,data = training_set)
modeloreg
summary(modeloreg)

modeloreg <- lm(medv ~ crim+zn+nox+rm+dis+rad+ptratio+lstat,data = training_set)
modeloreg
summary(modeloreg)

modeloreg <- lm(medv ~ crim+nox+rm+dis+ptratio+lstat,data = training_set)
modeloreg
summary(modeloreg)

modeloreg <- lm(medv ~ nox+rm+dis+ptratio+lstat,data = training_set)
modeloreg
summary(modeloreg)


library(ggplot2)
ggplot() + 
  geom_point(aes(x = training_set$crim,y = training_set$medv), colour='red') + geom_line(aes(x = training_set$crim, y =predict(modeloreg, newdata = training_set)), colour='blue') +
  ggtitle('something') + xlab('crim') + ylab('medv')

ggplot() + 
  geom_point(aes(x = test_set$crim,y = test_set$medv), colour='red') + geom_line(aes(x = test_set$crim, y =predict(modeloreg, newdata = test_set)), colour='blue') +
  ggtitle('something') + xlab('crim') + ylab('medv')