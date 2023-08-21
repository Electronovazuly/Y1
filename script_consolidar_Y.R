files <-list.files(setwd("C:/Users/yrincong/OneDrive - Falabella/Área Dataminig SARLAFT/5.Alertas/Año 2023/7.Julio/2.Insumos/Alta trx/insumos/"))

Base_Consolidado <- data.frame(NULL)


for (i in 1) {
  
  Base_Consolidado <- read.csv(paste0("C:/Users/yrincong/OneDrive - Falabella/Área Dataminig SARLAFT/5.Alertas/Año 2023/7.Julio/2.Insumos/Alta trx/insumos/",files[i]),sep = ";")
  
}
 
for (j in 2:length(files)) {
  
  Base_Consolidado <- rbind(Base_Consolidado,read.csv(paste0("C:/Users/yrincong/OneDrive - Falabella/Área Dataminig SARLAFT/5.Alertas/Año 2023/7.Julio/2.Insumos/Alta trx/insumos/",files[j]),sep = ";"))
  
} 

write.csv(Base_Consolidado,"C:/Users/yrincong/OneDrive - Falabella/Área Dataminig SARLAFT/5.Alertas/Año 2023/7.Julio/2.Insumos/Alta trx/insumos/consolidado_Abril_Alta_trx.csv", row.names = F)

