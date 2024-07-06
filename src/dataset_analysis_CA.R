require("data.table")
library(ggplot2)
library(dplyr)
library(mice)

client = 29200984

dataset <- fread('/Users/matiasvenditti/Desktop/expw_CA-0001_dataset.csv.gz')
Display_client_rent(dataset, client)

Corregir_MICE(dataset)
Display_client_rent(dataset, client)


Corregir_MICE <- function(dataset) {
  cat( "inicio Corregir_MICE()\n")
  
  imputed_data <- mice(dataset, m = 5, method = 'pmm', maxit = 50, seed = 100109)
  all_imputed_data = complete(imputed_data, action = 'all')
  
  average_data <- dataset
  
  # Calcula el promedio para cada columna por todos los datos imputados
  for (col in names(dataset)) {
    if (any(is.na(dataset[[col]]))) {  # Solo para columnas que tienen datos faltantes
      imputed_values <- sapply(all_imputed_data, function(x) x[[col]])
      average_data[[col]] <- rowMeans(imputed_values, na.rm = TRUE)
    }
  }
  
  dataset = average_data
  
  cat( "fin Corregir_MICE()\n")
}

Display_client_rent <- function(dataset, client) {
  filtered <- dataset[numero_de_cliente == client]
  
  df <- data.frame(foto_mes = filtered$foto_mes, mrentabilidad_annual = filtered$mrentabilidad_annual)
  df[is.na(df)] <- 0
  
  ggplot(df, aes(x = foto_mes, y = mrentabilidad_annual)) + 
    geom_line() + 
    geom_point() + 
    labs(title = paste("Cliente", client, sep = ' '), x = "Foto Mes", y = "Rent Anual") + 
    theme_minimal()
}
