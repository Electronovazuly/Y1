library(readxl)
reg <- read_excel("C:/Users/zulay/OneDrive/Escritorio/reg.xlsx")
View(reg)
library(neuralnet)
# Cargar la base de datos
data <- reg

# Dividir los datos en conjunto de entrenamiento y prueba (por ejemplo, 80% entrenamiento, 20% prueba)
set.seed(123)
indices_entrenamiento <- sample(1:nrow(data), 0.8 * nrow(data))
conjunto_entrenamiento <- data[indices_entrenamiento, ]
conjunto_prueba <- data[-indices_entrenamiento, ]

# Definir la fórmula para la red neuronal
formula <-  ~ X1 + X2 + X3 + X4 + X5 + X6

# Entrenar la red neuronal de dos capas
modelo <- neuralnet(formula, data = conjunto_entrenamiento, hidden = c(5), linear.output = TRUE)

# Realizar predicciones en el conjunto de prueba
predicciones <- compute(modelo, conjunto_prueba[, 1:6])

# Calcular el error cuadrático medio de raíz (RMSE)
rmse <- sqrt(mean((predicciones$net.result - conjunto_prueba$Y6)^2))

resultados <- data.frame(Predicciones = predicciones$net.result, ValorReal = conjunto_prueba$Y6)
print(resultados)
print(rmse)

ggplot(resultados, aes(x = ValorReal, y = Predicciones)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Valor Real", y = "Predicciones") +
  ggtitle("Comparación entre Predicciones y Valores Reales")


# Gráfico de distribución de los errores
errores <- predicciones$net.result - conjunto_prueba$Y1
ggplot(data.frame(errores), aes(x = errores)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(x = "Error", y = "Frecuencia") +
  ggtitle("Distribución de Errores")

library(car)
library(corrplot)
# Pruebas de hipótesis
# Supongamos que queremos probar la hipótesis de que el coeficiente de X1 es igual a cero
# Realizamos la prueba t utilizando el paquete "car"
hipotesis <- linearHypothesis(modelo, hypothesis.matrix = "X1 = 0")
print(hipotesis)

# Si el valor p es menor que el nivel de significancia (por ejemplo, 0.05), rechazamos la hipótesis nula

# Pruebas de independencia
# Supongamos que queremos probar la independencia entre X1 y X2
# Utilizamos la prueba de independencia de Durbin-Watson utilizando el paquete "car"
independencia <- durbinWatsonTest(modelo)
print(independencia)

# Si el valor estadístico es cercano a 2, no hay evidencia de autocorrelación

# Multicolinealidad aproximada
# Calculamos la matriz de correlación entre las variables predictoras
matriz_correlacion <- cor(conjunto_entrenamiento[, c("X1", "X2", "X3", "X4", "X5", "X6")])
corrplot(matriz_correlacion, method = "circle")

# Si existen correlaciones altas (por ejemplo, coeficiente de correlación > 0.7), puede haber multicolinealidad aproximada entre las variables predictoras


matriz_correlacion <- cor(data[, c("X1", "X2", "X3", "X4", "X5", "X6")])
library(corrplot)
corrplot(matriz_correlacion, method = "color")
library(car)
vif_modelo <- vif(modelo)
