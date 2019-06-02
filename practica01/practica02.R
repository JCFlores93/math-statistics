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
library(rpart)

train<-read.csv("Morosidad.csv",sep = ';',encoding="UTF-8",  header = T, fileEncoding = "cp932") # leer la data de entrenamiento
head(train)

names(train) # visualizar los nombres de la data
head(train)  # visualizar los 6 primeros registros
str(train)   # ver la estructura de la data

summary(train) # tabla comun de obtener
summarizeColumns(train) # tabla mas completa

resumen=data.frame(summarizeColumns(train))

dim(train)
unique(train$Nro_Veces_cob)
unique(train$Nro_Cuotas)
unique(train$Mes_pres)
unique(train$Llamada_fecha)
unique(train$Llamada_Resultado)
unique(train$Estatus)
unique(train$Tipo_contacto)
unique(train$Cod_cliente)

drop <- c("Llamada_fecha", "Hora", "Llamada_resultado","Cod_cliente")
train = train[,!(names(train) %in% drop)]

hist(train$Cod_cliente, breaks = 100, main = "Cod_cliente",xlab = "Cod_cliente",col="blue")

drop <- c("Llamada_fecha", "Hora", "Llamada_resultado","Cod_cliente")
train = train[,!(names(train) %in% drop)]

library(dummies)
dummy_data <- dummy.data.frame(train, sep = ".")

names(dummy_data)
drop <- c("Nro_Veces_cob.<=10", "Nro_Cuotas.>48", "Mes_pres.Marzo" ,"MES_2_Atraso","Llamada_Resultado.TELF. NO CORRESPONDE", "Tipo_contacto.CNE")
dummy_data = dummy_data[,!(names(dummy_data) %in% drop)]

summarizeColumns(dummy_data) # tabla mas completa
is.na(dummy_data)

set.seed(123)
split = sample.split(dummy_data$Mora, SplitRatio= 2/3)
training_set = subset(dummy_data, split==TRUE)
test_set = subset(dummy_data, split==TRUE)

classifier <- rpart(Mora~., data = training_set)
library(tidyverse)
y_pred = predict(classifier, newdata=test_set)
table(test_set$Mora, y_pred)

library(rpart.plot)
prp(classifier, extra = 1, type = 1)

library(partykit)
plot(as.party(classifier))
plot(classifier)
names(classifier)

predict <- predict(classifier, test_set, type = "vector")

