prestamo <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/Mineria-de-datos/master/Prestamo.csv', sep = ';')
head(prestamo)
summary(prestamo)
str(prestamo)
names(prestamo)

##################################
library(ggplot2)
ggplot(data=prestamo) +
  geom_bar(mapping= aes(x=Edad, fill=Prestamo))
library(gmodels)
CrossTable(prestamo$Edad, prestamo$Prestamo, chisq = T)

# Genero
ggplot(data=prestamo) +
  geom_bar(mapping= aes(x=Genero, fill=Prestamo))
CrossTable(prestamo$Genero, prestamo$Prestamo, chisq = T)

library(rpart)
#modelo1<-glm(prestamo~)

##########################################################################################################
morosidad <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/Morosidad.csv', sep = ';')
head(morosidad)

str(morosidad)
morosidad$Mora<- as.factor(morosidad$Mora)
summary(morosidad)

names(morosidad)
modelolog <- glm(data=morosidad,Mora~Nro_Cuotas + MES_0_Atraso+ Deuda_total, family="binomial")
summary(modelolog)

#INTERPRETANDO LOS COEFICIENTES
# - Para cada cambio en el logaritmo del numero de cuotas
# entre 24 y 48 cuotas aumenta en 0.4 el logaritmo de la morosidad (y=1)

#####################################################################################################################

# Por cada cambio en el logaritmo del numero de cuotas entre
# <24, 48] la ventaja/ratio/probabilidad de ser moroso aumenta en 0.4 = 4.572e-01(respecto a)
# comparacion con los clientes cuyo número de cuotas <24

# Por cada cambio en el logaritmo del numero de cuotas entre
# >48 la ventaja/ratio/probabilidad de ser moroso aumenta en 0.6... = 6.160e-01(respecto a)
# comparacion con los clientes cuyo número de cuotas <24

confint(modelolog)
coeficientes <- coef(modelolog)
summary(coeficientes)

#Obtener la probabilidad
exp(coeficientes)

lost_client <- read.csv('https://raw.githubusercontent.com/VictorGuevaraP/ME-Machine-Learning/master/Perdida%20de%20clientes.csv', sep = ';')
head(lost_client)

str(lost_client)
names(lost_client)
modelolog1 <- glm(data=lost_client,Desafiliado~., family="binomial")
summary(modelolog1)

confint(modelolog1)
coeficientes1 <- coef(modelolog1)
summary(coeficientes1)

#Obtener la probabilidad
exp(coeficientes1)

library(MASS)
# Significancia de cada variable
stepAIC(modelolog1)