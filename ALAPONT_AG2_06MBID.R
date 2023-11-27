# install.packages("ggplot2")
# install.packages("stats")
# install.packages("forecast")
library(ggplot2)
library(stats)
library(forecast)
# Convertir la columna de fecha a formato POSIXct
temporal_dataset$Date <- as.POSIXct(temporal_dataset$Date, format="%m/%d/%Y %H:%M")

# Ordenar el conjunto de datos por la columna de fechas
temporal_dataset <- temporal_dataset[order(temporal_dataset$Date), ]

# Crear un gráfico ggplot
ggplot(temporal_dataset, aes(x = Date, y = Temp_C)) +
  geom_line() +
  labs(x = "Fecha", y = "Temperatura (°C)", title = "Serie Temporal de Temperatura")
# Resumen estadístico de la temperatura
summary(temporal_dataset$Temp_C)

# Media de la temperatura
mean_temp <- mean(temporal_dataset$Temp_C)
cat("Media de la temperatura:", mean_temp, "°C\n")

# Mediana de la temperatura
median_temp <- median(temporal_dataset$Temp_C)
cat("Mediana de la temperatura:", median_temp, "°C\n")

# Desviación estándar de la temperatura
sd_temp <- sd(temporal_dataset$Temp_C)
cat("Desviación estándar de la temperatura:", sd_temp, "°C\n")

# Percentiles
percentiles <- c(5, 95)
temp_percentiles <- quantile(temporal_dataset$Temp_C, c(percentiles / 100))
cat("Percentiles 5 y 95 de la temperatura:", temp_percentiles, "°C\n")

#Descomposición de la serie temporal

# Crear la serie temporal
ts_temporal <- ts(temporal_dataset$Temp_C, frequency = 24)

# Realizar la descomposición
decomposition <- stl(ts_temporal, s.window = "periodic")

# Visualizar la descomposición
plot(decomposition)

# Acceder a las componentes individuales
trend <- decomposition$time.series[, "trend"]
seasonal <- decomposition$time.series[, "seasonal"]
residuals <- decomposition$time.series[, "remainder"]

# Análisis de la tendencia
plot(trend)

# Análisis de la serie estacional
plot(seasonal, main = "Componente Estacional", ylab = "Estacionalidad")

# Análisis de los residuos
plot(residuals, main = "Residuos", ylab = "Residuos")

#Análisis de autocorrelación

# Calcular la autocorrelación
acf_resultado <- acf(temporal_dataset$Temp_C, lag.max = 24, main = "Función de Autocorrelación")

# Visualizar los resultados
plot(acf_resultado)

#Aplicación de un modelo adecuado

# Especificar los órdenes del modelo ARIMA
p <- 1  # Orden AR
d <- 1  # Diferenciación
q <- 1  # Orden MA

P <- 1  # Orden AR estacional
D <- 1  # Diferenciación estacional
Q <- 1  # Orden MA estacional
# Crear la serie temporal
ts_temporal <- ts(temporal_dataset$Temp_C, frequency = 24)

# Ajustar un modelo ARIMA
modelo_arima <- arima(ts_temporal, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = 24))

# Resumen del modelo
summary(modelo_arima)

# Abrir una nueva ventana de gráficos con dimensiones más grandes
windows(width = 10, height = 6)

# Visualizar diagnósticos del modelo
tsdiag(modelo_arima)

# Especificar diferentes órdenes del modelo ARIMA
p <- 1  # Orden AR
d <- 1  # Diferenciación
q <- 1  # Orden MA
P <- 1  # Orden AR estacional
D <- 1  # Diferenciación estacional
Q <- 1  # Orden MA estacional

# Ajustar el modelo ARIMA con los nuevos órdenes
modelo_arima_mejorado <- arima(ts_temporal, order = c(p, d, q), seasonal = list(order = c(P, D, Q), period = 24))

# Resumen del nuevo modelo
summary(modelo_arima_mejorado)

# Abrir una nueva ventana de gráficos con dimensiones más grandes
windows(width = 10, height = 6)

# Visualizar diagnósticos del modelo
tsdiag(modelo_arima_mejorado)
