library(readxl)
library(fpp2)
library(dplyr)
library(lubridate)
library(tidyr)
library(forecast)
library(tseries)

ruta_excel <- "C:\\Users\\ani20\\OneDrive\\Escritorio\\Futuros.xlsx"
datos <- read_excel(ruta_excel)

# Transformar los datos de la gasolina de días a meses
gasto_mensual <- datos %>%
  mutate(month = format(Fecha, "%m"), year = format(Fecha, "%Y")) %>%
  group_by(month, year) %>%
  summarise(Gasolina = mean(Gasolina))

# Crea la nueva base con el promedio del gasto mensual en gasolina
gasto_mensual <- gasto_mensual[with(gasto_mensual, order(gasto_mensual$year)), ]

# Convertir los datos de gasolina en una serie de tiempo
serie_gasolina <- ts(gasto_mensual$Gasolina, start = c(2015, 1), frequency = 12) # Frecuencia mensual

# Graficar la serie original
plot(serie_gasolina, main = "Serie de Precios de Gasolina", ylab = "Precio (USD/Litro)", xlab = "Fecha")

# Verificar estacionariedad con prueba de Dickey-Fuller
adf.test <- adf.test(serie_gasolina)
print(adf.test)

# Si la serie no es estacionaria, aplicamos diferencias
serie_diff <- diff(serie_gasolina, differences = 1)

# Graficar la serie diferenciada
plot(serie_diff, main = "Serie diferenciada de gasolina", ylab = "Diferencias del Precio", xlab = "Fecha")

# Ajustar un modelo ARIMA automáticamente
modelo_arima <- auto.arima(serie_gasolina, seasonal = TRUE, trace = TRUE)
summary(modelo_arima)

# Realizar el pronóstico para los próximos 12 meses
pronostico <- forecast(modelo_arima, h = 12)

# Visualizar el pronóstico
plot(pronostico, main = "Pronóstico de Precios de Gasolina a 12 Meses", 
     xlab = "Fecha", ylab = "Precio (USD/Litro)")

# Mostrar los valores del pronóstico
print(pronostico)

# Guardar el pronostico como DataFrame
Gas <- as.data.frame(pronostico)

# Agregar los valores del futuro segun yahoo a la nueva Data
Futuro <- c(69.91, 69.58, 69.11, 68.74, 68.44, 68.19, 67.92, 67.59, 66.74, 
            67.29, 67.02, 66.56)
Gas$Futuro <- Futuro

# Transformar los datos del petroleo de días a meses
petroleo_mensual <- datos %>%
  mutate(month = format(Fecha, "%m"), year = format(Fecha, "%Y")) %>%
  group_by(month, year) %>%
  summarise(Petroleo = mean(Petroleo))

petroleo_mensual <- petroleo_mensual[with(petroleo_mensual, order(petroleo_mensual$year)), ]