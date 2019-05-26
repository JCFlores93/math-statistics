#Cargamos el conjunto de datos 
telco <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/Perdida%20de%20clientes.csv', sep = ';')
head(telco)

barplot(table(telco$Desafiliado))
dim(telco)

set.seed(123)
muestra <- sample(2333, 1000) 
head(muestra)
length(muestra)

train<-telco[-muestra,]
test<-telco[muestra,]

dim(train)
dim(test)

summary(train)
summary(test)

names(train)
library(gmodels)
CrossTable(train$Plan_Internacional, train$Desafiliado, chisq = T)

?CrossTable

modeloreg <- lm(Desafiliado~.,data = train)
modeloreg

summary(modeloreg)

head(test)

modeloregl <- glm(Desafiliado~.,data = train, family = "binomial")
modeloregl

summary(modeloregl)

head(telco)
#Modelos de clasificacion
#Mediante la libreria rpart
library(rpart)
set.seed(123)
modeloa1 <- rpart(Desafiliado~., data=train, method = 'class')
modeloa1

library(rpart.plot)
prp(modeloa1, extra = 1, type = 1)

library(partykit)
plot(as.party(modeloa1))
plot(modeloa1)
names(modeloa1)

prediccion1 <- predict(modeloa1,train, type = "class")
table(prediccion1,train$Desafiliado)

prediccion2 <- predict(modeloa1,test, type = "class")
table(prediccion2,test$Desafiliado)

library(caret)
library(e1071)
confusionMatrix(prediccion1, train$Desafiliado)
confusionMatrix(prediccion2, test$Desafiliado)


modeloa2 <- rpart(Desafiliado~., data=train, method = 'class', cp=0, minbucket=5)
modeloa2
prp(modeloa2, extra = 1, type = 1)
plot(as.party(modeloa2))
plot(modeloa2)

prediccion2 <- predict(modeloa2,train, type = "class")
table(prediccion2,train$Desafiliado)

prediccion3 <- predict(modeloa2,test, type = "class")
table(prediccion3,test$Desafiliado)
confusionMatrix(prediccion2, train$Desafiliado)
confusionMatrix(prediccion3, test$Desafiliado)
#############

errores <- modeloa2$cptable[, "xerror"]
minerror<-which.min(errores)
mincp<-modeloa2$cptable[minerror, "CP"]
arbolcortado<-prune(modeloa2, cp=mincp)

plot(as.party(arbolcortado))
prp(arbolcortado, extra = 1, type = 1)
arbolcortado