library(readxl)
library(tidyverse)
library(ggplot2)

# Buscar la ruta del archivo de excel
file.choose()

# Guardar la ruta en una variable
ruta_excel <- "C:\\Users\\ani20\\OneDrive\\Escritorio\\Pronostico.xlsx"
datos <- read_excel(ruta_excel)

# Calcular diferencias de primer orden
datos$Delta_Petroleo <- diff(c(NA, datos$Petroleo))  # Diferencia en petróleo
datos$Delta_Gasolina <- diff(c(NA, datos$Gasolina))  # Diferencia en gasolina

# Eliminar la primera fila con valores NA
datos_dif <- na.omit(datos)

# Ajustar el modelo de regresión lineal
modelo <- lm(Delta_Gasolina ~ Delta_Petroleo, data = datos_dif)

# Resumen del modelo
summary(modelo)

beta_cobertura <- coef(modelo)[2]  # Extraer coeficiente de cobertura (pendiente)
print(paste("Beta de cobertura:", round(beta_cobertura, 3)))

# Simulación
cantidad_gasolina <- 17000  # Cantidad de gasolina a cubrir (en litros)

# Calcular el costo acumulado sin cobertura
datos_dif$Costo_SinCobertura <- datos_dif$Delta_Gasolina * cantidad_gasolina

# Calcular el costo acumulado con cobertura
datos_dif$Costo_ConCobertura <- (datos_dif$Delta_Gasolina - beta_cobertura * datos_dif$Delta_Petroleo) * cantidad_gasolina

# Cálculo acumulado de los costos
datos_dif$Costo_Total_SinCobertura <- cumsum(datos_dif$Costo_SinCobertura)
datos_dif$Costo_Total_ConCobertura <- cumsum(datos_dif$Costo_ConCobertura)

# Visualizar resultados
ggplot(datos_dif, aes(x = Fecha)) +
  geom_line(aes(y = Costo_Total_SinCobertura, color = "Sin Cobertura"), size = 1) +
  geom_line(aes(y = Costo_Total_ConCobertura, color = "Con Cobertura"), size = 1) +
  labs(title = "Simulación de Cobertura: Comparación de Costos",
       x = "Fecha", y = "Costo Acumulado (USD)") +
  scale_color_manual(values = c("Sin Cobertura" = "#42C233", "Con Cobertura" = "#0C29B0")) +
  theme_minimal()
