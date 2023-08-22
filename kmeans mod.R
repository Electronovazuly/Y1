ruta_data <-  "C:\\Users\\zulay\\OneDrive\\Escritorio\\Nueva carpeta\\data.xlsx"

data <- read_excel(ruta_data)
data_normalizada <- scale(data)
head(data_normalizada)
### calculo de distancias ###
library(factoextra)
library(cluster)
library(ggplot2)
library(tidyverse)
library(NbClust)
m.distancia <- get_dist(data_normalizada, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low = "blue", mid = "white", high = "red"))
## estimación de numero de cluster###
# Elbow silhouette o gap_stat method
fviz_nbclust(data_normalizada, kmeans, method = "wss")

fviz_nbclust(data_normalizada, kmeans, method = "silhouette")

fviz_nbclust(data_normalizada, kmeans, method = "gap_stat")

#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).


clusterkm <- NbClust(data_normalizada, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "kl")
fviz_nbclust(clusterkm)

fviz_nbclust(data , kmeans, method = "silhouette")
#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust(df, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)
library(NbClust)
#con esta función se pueden calcular:
#the index to be calculated. This should be one of : "kl", "ch", "hartigan", "ccc", "scott",
#"marriot", "trcovw", "tracew", "friedman", "rubin", "cindex", "db", "silhouette", "duda",
#"pseudot2", "beale", "ratkowsky", "ball", "ptbiserial", "gap", "frey", "mcclain", "gamma",
#"gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw", "all" (all indices except GAP,
#Gamma, Gplus and Tau), "alllong" (all indices with Gap, Gamma, Gplus and Tau included).
resnumclust<-NbClust (data , distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "all")
fviz_nbclust(resnumclust)
resnumclust<-NbClust (data_normalizada , distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "all")
fviz_nbclust(resnumclust)
resnumclust<-NbClust(data, distance = "euclidean", min.nc=2, max.nc=10, method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)


