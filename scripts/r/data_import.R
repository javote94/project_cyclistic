import_data_csv <- function(path = "data/raw/csv/") {
  
  # Obtener las rutas relativas de todos los archivos .csv
  csv_files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
  
  # Leer cada archivo .csv y alojarlo en una lista de dataframes
  df_list <- lapply(csv_files, read_csv)
  
  # Nombrar los elementos de la lista según el nombre del archivo
  names(df_list) <- gsub("^.*/|\\.csv$", "", csv_files)
  
  # Retornar la lista de data frame
  return(df_list)
}