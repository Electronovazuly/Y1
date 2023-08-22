ruta_data <- "C:\Users\zulay\OneDrive\Escritorio\Nueva carpeta\\data.xls"
data <- read_excel(ruta_data)
file.choose()
ruta_data <-  "C:\\Users\\zulay\\OneDrive\\Escritorio\\Nueva carpeta\\data.xlsx"
data <- read_excel(ruta_data)
data_normalizada <- scale(data)
head(data_normalizada)
### calculo de distancias ###
get_dist(data_normalizada, method = "euclidean")
