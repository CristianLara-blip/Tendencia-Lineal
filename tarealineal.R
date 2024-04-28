# Limpiar el entorno de trabajo
rm(list = ls())

# Cargar paquetes necesarios
library(readxl)
library(seasonal)

# Paso 1: Importar datos desde Excel
datos_excel <- read_excel("G:/Ventas trimestrales.xlsx")
View(datos_excel)


# Paso 2: Calcular los índices estacionales
datos_ts <- ts(datos_excel$Ventas, frequency = 4, start = c(2007, 1))
indices_estacionales <- decompose(datos_ts)$seasonal / decompose(datos_ts)$trend

# Mostrar resultados
print("Índices estacionales:")
print(indices_estacionales)

# Gráfico de índices estacionales
plot(indices_estacionales, type = "l", main = "Índices estacionales", xlab = "Trimestre", ylab = "Índice estacional")



# Paso 3: Desestacionalizar las mediciones originales
datos_excel$Ventas_desestac <- datos_excel$Ventas / indices_estacionales

# Mostrar resultados
print("Desestacionalización de las ventas originales:")
print(datos_excel$Ventas_desestac)



# Paso 4: Calcular una ecuación de regresión para las ventas trimestrales
modelo <- lm(Ventas ~ Año + Trimestre, data = datos_excel)

# Mostrar resultados
print("Ecuación de regresión:")
summary(modelo)
View(modelo)


# Paso 5: Pronosticar las ventas trimestrales para los trimestres de 2011
ventas_2011 <- data.frame(
  Año = rep(2011, 4),
  Trimestre = c(1, 2, 3, 4)
)
View(ventas_2011)

ventas_2011$Pronostico <- predict(modelo, newdata = ventas_2011)
View(ventas_2011)


# Paso 6: Ajustar los pronósticos utilizando los índices estacionales
# Filtrar los índices estacionales para el año 2011
indices_2011 <- indices_estacionales[49:52] # Suponiendo que los datos comienzan en el año 2007

# Ajustar los pronósticos utilizando los índices estacionales para 2011
ventas_2011$Pronostico_ajustado <- ventas_2011$Pronostico * indices_2011

# Gráfico de pronósticos para 2011
plot(ventas_2011$Pronostico, type = "l", main = "Pronósticos para 2011", xlab = "Trimestre", ylab = "Ventas Pronosticadas")
lines(ventas_2011$Pronostico_ajustado, col = "red")
legend("topright", legend = c("Pronóstico Original", "Pronóstico Ajustado"), col = c("black", "red"), lty = 1)

# Mostrar resultados
print("Pronósticos para 2011:")
print(ventas_2011)
