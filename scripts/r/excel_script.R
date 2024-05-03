
create_and_save_excel <- function(file_path, ...) {
  
  # Crea un nuevo libro de Excel
  wb <- createWorkbook()
  
  # Lista para contener las tablas pasadas a la función
  tables <- list(...)
  
  # Iterar sobre cada tabla y escribirla en una nueva hoja
  for (i in seq_along(tables)) {
    
    # Añade una nueva hoja por cada tabla
    sheet_name <- paste("Tabla", i)  # Nombra las hojas como Tabla 1, Tabla 2, etc.
    addWorksheet(wb, sheet_name)
    
    # Escribe la tabla actual en la hoja
    writeData(wb, sheet = sheet_name, x = tables[[i]], startRow = 1, startCol = 1)
  }
  
  # Guarda el libro de Excel en la ruta especificada
  saveWorkbook(wb, file_path, overwrite = TRUE)
}
