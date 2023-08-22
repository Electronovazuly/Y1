### 1. Preparación de datos 
library(readr)
BASE_1 <- read_delim("C:/Users/zulay/OneDrive/Escritorio/Prueba Red/Prueba Red/BASE 1.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(BASE_1)

## unificación descripción de la actividad economica de cada cliente
library(readxl)
BASE_2 <- read_excel("C:/Users/zulay/OneDrive/Escritorio/Prueba Red/Prueba Red/BASE 2.xlsx")
View(BASE_2)

library(rmarkdown)
library(dplyr)

basetotal <- inner_join(BASE_1, BASE_2, "Código de actividad")
colnames(BASE_2)
View(basetotal)  
View(basetotal)

data_normalizada <- scale (basetotal)
View(basetotal)
###### ANALISIS DE DATOS
### CORRELACION DE VARIABLES 
# COEFICIENCIETE DE CORRELACIÓN PEARSON
colnames(basetotal)
data1 <- data.frame ("Código Cliente", "Código de actividad" , "Mes 1 Cantidad Operacione", "Mes 1 Valor Total Operaciones", "Mes 1 Cantidad Operaciones Tarjetas Internacionales", "Mes 1 Valor Operaciones Tarjetas Internacionales", "Mes 2 Cantidad Operaciones", "Mes 2 Valor Total Operaciones" , "Mes 2 Cantidad Operaciones Tarjetas Internacionales", "Mes 2 Valor Operaciones Tarjetas Internacionales") 
View(data1)
data2 <- c("Código Cliente", "Código de actividad" , "Mes 1 Cantidad Operacione", "Mes 1 Valor Total Operaciones", "Mes 1 Cantidad Operaciones Tarjetas Internacionales", "Mes 1 Valor Operaciones Tarjetas Internacionales", "Mes 2 Cantidad Operaciones", "Mes 2 Valor Total Operaciones" , "Mes 2 Cantidad Operaciones Tarjetas Internacionales", "Mes 2 Valor Operaciones Tarjetas Internacionales") 
View(data2)
data3 <- select (basetotal,"Código Cliente", "Código de actividad" , "Mes 1 Cantidad Operaciones", "Mes 1 Valor Total Operaciones", "Mes 1 Cantidad Operaciones Tarjetas Internacionales", "Mes 1 Valor Operaciones Tarjetas Internacionales", "Mes 2 Cantidad Operaciones", "Mes 2 Valor Total Operaciones" , "Mes 2 Cantidad Operaciones Tarjetas Internacionales", "Mes 2 Valor Operaciones Tarjetas Internacionales" )
View(data3)
library(corrplot)
library(foreign)
library(apaTables)
library(corrr)
library(PerformanceAnalytics)
library(psych)

cor(data3)
correlac <- cor(data3)
corrplot(correlac, method = "ellipse")
View(data3)
data4 <- select(basetotal, "Código Cliente" , "Mes 1 Cantidad Operaciones", "Mes 1 Valor Total Operaciones", "Mes 1 Cantidad Operaciones Tarjetas Internacionales", "Mes 1 Valor Operaciones Tarjetas Internacionales", "Mes 2 Cantidad Operaciones", "Mes 2 Valor Total Operaciones" , "Mes 2 Cantidad Operaciones Tarjetas Internacionales", "Mes 2 Valor Operaciones Tarjetas Internacionales" )

### Normalización de datos 
data_normalizada <- scale(data3)

#### correlación 
chart.Correlation(data_normalizada)

#### graficar
plot(data3$`Mes 1 Valor Total Operaciones`, data3$`Mes 1 Valor Total Operaciones` )

### pruebas de normalidad 
hist(data3$`Mes 1 Valor Total Operaciones`, col = "blue")

### 2. Kmeans
library(factoextra)
library(cluster)
library(ggplot2)
library(tidyverse)
library(NbClust)

#### KMEANS fviz_cluster, hcut, hkmeans, eclust, fviz_dend
m.distancia <- get_dist(data_normalizada, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))

### determinar el No, de cluster 

fviz_nbclust(data_normalizada, kmeans, method = "wss")## primer método
fviz_nbclust(data_normalizada, kmeans, method = "silhouette") ## segundo método
fviz_nbclust(data_normalizada, kmeans, method = "gap_stat")  ## tercer método
resnumclust<-NbClust (data_normalizada , distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "all")
fviz_nbclust(resnumclust)## cuarto método

## Clustering

km.res <- kmeans(data_normalizada, 4, nstart = 2)### Cuatro cluster dos centroides,

## visualización KMEANS

fviz_cluster(km.res, data_normalizada[, -5], ellipse.type = "norm")+
  theme_minimal()

# Visualize silhouhette information
require("cluster")
sil <- silhouette(km.res$cluster, dist(data_normalizada))
fviz_silhouette(sil)

# Identify observation with negative silhouette
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

if (FALSE) {
  # PAM clustering
  # ++++++++++++++++++++
  require(cluster)
  pam.res <- pam(data_normalizada, 3)
  # Visualize pam clustering
  fviz_cluster(pam.res, ellipse.type = "norm")+
    theme_minimal()
  # Visualize silhouhette information
  fviz_silhouette(pam.res)
  
  # Hierarchical clustering
  # ++++++++++++++++++++++++
  # Use hcut() which compute hclust and cut the tree
  hc.cut <- hcut(data_normalizada, k = 3, hc_method = "complete")
  # Visualize dendrogram
  fviz_dend(hc.cut, show_labels = FALSE, rect = TRUE)
  # Visualize silhouhette information
  fviz_silhouette(hc.cut)
}

## 3. Analisis descriptivo de datos 

#pruebas de normalidad de shapiro wilks con la variable cantidad de operaciones mes 1

prueba_sha <- shapiro.test(data3$`Mes 1 Cantidad Operaciones`)
print(prueba_sha)

## pruebas de correlación 

pairs.panels(data3, 
             method = "pearson", 
             density = FALSE, 
             ellipses = FALSE, 
             smooth = FALSE
             )

## Calculo de correlación que existe entre las variables 
data_cor <- cor(data3, method = "pearson")


## aproximaciones de correlación 
data_cor <- round(data_cor, digits = 2)

### grafica de mapa de calor 
library(ggcorrplot)

ggcorrplot(data_cor, method = 'circle', type = 'lower', lab = TRUE ) + 
  ggtitle("matriz de correlación")+
  theme_minimal()
## significancia de la correlación 
corr.test(data3, method = "pearson", adjust = "none")


###4. pruebas de tendencia Modelo de regresión 

summary (data3)

colnames(data3)
