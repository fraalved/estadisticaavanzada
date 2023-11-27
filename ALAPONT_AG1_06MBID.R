# Instala y carga las librerías necesarias
install.packages(c("tidyverse", "MASS", "boot", "car", "ggplot2", "QuantPsyc" , "gridExtra"))
library(tidyverse)
library(QuantPsyc)
library(MASS)
library(ISLR)
library(boot)
library(car)
library(ggplot2)
library(gridExtra)


# Carga y visualiza los datos de flujos de vehículos
View(flujos)
head(flujos)
names(flujos)
attach(flujos)

# Exploración de las variables
summary(flujos)
str(flujos)

#seleccionamos las variables#
str(averageVehicleSpeed)
str(averageDistanceHeadway)
str(percentageLongVehicles)
str(vehicleFlow)
str(occupancy)

#Grafico de correlacion de multiples variables##
pairs(flujos[,1:5])
pairs(flujos[,1:2])

#relacion entre variables#
cor(vehicleFlow,averageDistanceHeadway)
cov(vehicleFlow,averageDistanceHeadway)
cor.test(vehicleFlow,averageDistanceHeadway)

##regresion lineal de variables modelo 1 ##
modelo1=lm(vehicleFlow~averageDistanceHeadway,data=flujos)
summary(modelo1)

#relacion entre variables#
cor(vehicleFlow,averageVehicleSpeed)
cov(vehicleFlow,averageVehicleSpeed)
cor.test(vehicleFlow,averageVehicleSpeed)

##regresion lineal de variables modelo 2 ##
modelo2=lm(vehicleFlow~averageVehicleSpeed,data=flujos)
summary(modelo2)

# Matriz de correlación entre variables independientes
cor_matrix <- cor(data[, -5])  # Excluir la variable dependiente (vehicleFlow)

# VIF para cada variable independiente
library(car)
vif_values <- vif(modelo)

# Imprimir la matriz de correlación
print("Matriz de correlación:")
print(cor_matrix)

# Imprimir los valores VIF
print("Valores VIF:")
print(vif_values)

# Regresión Multilineal y estadísticas de variables

# Ajustar modelo de regresión
modelo <- lm(vehicleFlow ~ averageVehicleSpeed + averageDistanceHeadway + percentageLongVehicles + occupancy, data = flujos)
summary(modelo)

# Observar el modelo utilizado
names(modelo)

# Coeficientes del modelo
coef(modelo)

# Calcular intervalos de confianza
confint(modelo)

# Inferencia de regresión con nivel de confianza del 90%
confint(modelo)
confint(modelo, level = 0.90)

# Predicciones
# Calcular predicciones para nuevas observaciones
nuevas_observaciones <- data.frame(averageVehicleSpeed = c(110, 95, 120),
                                   averageDistanceHeadway = c(60, 45, 55),
                                   percentageLongVehicles = c(20, 15, 25),
                                   occupancy = c(5, 3, 6))
predict(modelo, nuevas_observaciones)

# Gráficos

# Plot de dispersión y recta de regresión
plot(flujos$averageVehicleSpeed, flujos$vehicleFlow, xlab = 'averageVehicleSpeed', ylab = 'vehicleFlow')

# Intervalos de confianza de la respuesta media
ic <- predict(modelo, interval = 'confidence')
lines(flujos$averageVehicleSpeed, ic[, 2], lty = 2)
lines(flujos$averageVehicleSpeed, ic[, 3], lty = 2)

# Gráfico de Intervalos de predicción
ic <- predict(modelo, interval = 'prediction')
lines(flujos$averageVehicleSpeed, ic[, 2], lty = 2, col = 'red')
lines(flujos$averageVehicleSpeed, ic[, 3], lty = 2, col = 'red')

# Tabla de análisis de la varianza (ANOVA)
anova(modelo)

# Diagnóstico de residuos
residuos <- residuals(modelo)
valores_ajustados <- fitted(modelo)
plot(valores_ajustados, residuos)

# QQ-plot para evaluar la normalidad de los residuos
qqnorm(residuos)
qqline(residuos)

# Gráficos adicionales
# Estadísticas de las variables
library(ggplot2)
ggplot(data = flujos, aes(x = averageVehicleSpeed, y = averageDistanceHeadway)) + geom_point() + labs(title = "averageVehicleSpeed y averageDistanceHeadway", x = "averageVehicleSpeed", y = "averageDistanceHeadway")
ggplot(data = flujos, aes(x = averageVehicleSpeed, y = averageDistanceHeadway, col = vehicleFlow)) + geom_point() + labs(title = "averageVehicleSpeed y averageDistanceHeadway", x = "averageVehicleSpeed", y = "averageDistanceHeadway", caption = "Regresion multineal")

