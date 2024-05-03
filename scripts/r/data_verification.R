
check_compatibility <- function(df_list) {
  
  df_list <- lapply(df_list, clean_names)
  
  # Tomar como referencia el vector de nombre de columnas del primer data frame de la lista
  reference_column_names <- colnames(df_list[[1]])
  
  # Verificar si todos los dataframes tienen la misma cantidad de columnas que el primero
  same_number_of_columns <- all(
    sapply(
      df_list,
      function(df)
        length(colnames(df)) == length(reference_column_names)
    )
  )
  
  if (same_number_of_columns) {
    n_columns <- length(reference_column_names)
    print(glue("1° verification: All datasets have {n_columns} columns"))
  } else {
    print("1° verification: The number of columns are different")
  }
  
  # Verificar coincidencia en el orden y nombre de las columnas en todos los dataframes
  same_name_and_order <- all(
    sapply(
      df_list,
      function(df)
        all(colnames(df) == reference_column_names)
    )
  )
  
  if (same_name_and_order) {
    print("2° verification: The order and names of the columns are the same")
  } else {
    print("2° verification: Column order or names are different")
  }
  
  # Tomar como referencia el vector de tipo de datos de las columnas del primer data frame de la lista
  reference_column_datatype <- sapply(df_list[[1]], function(col) class(col)[1])
  
  # Verificar coincidencia en el tipo de dato de las columnas en todos los dataframes
  same_datatype <- all(
    sapply(
      df_list,
      function(df)
        all(sapply(df, function(col) class(col)[1]) == reference_column_datatype)
    )
  )
  
  if (same_datatype) {
    print("3° verification: Column data types are supported")
  } else {
    print("3° verification: Column data types are not supported")
  }
  
  # Combinar las verificaciones de cantidad, nombre, orden y tipo de dato de las columnas
  all_checks_passed <- same_number_of_columns && same_name_and_order && same_datatype
  
  #Indicar al usuario en consola el resultado de la verificación
  if (all_checks_passed) {
    print("Result: Data frames are supported to combine")
  } else {
    print("Result: Data frames are not supported to merge")
  }
  
}

verify_data <- function(df) {
  
  message("Number of rows of the dataframe grouped by year and month (quantity and percentage)")
  df %>% 
    mutate(year = year(started_at),
           month = month(started_at, label = TRUE)) %>%
    group_by(year, month) %>%
    summarise(count = n(), .groups = "drop") %>%
    arrange(year, month) %>%
    ungroup() %>%
    mutate(
      percentage = (count/sum(count))*100,
      year = as.character(year)
    ) %>%
    bind_rows(
      summarise(., year = "", month = "Total", count = sum(count), percentage = 100)
    ) %>%
    print()
  
  type_of_data <- sapply(df, function(col) class(col)[1])
  na_counts <- sapply(df, function(col) sum(is.na(col)))
  message("Dataframe fields: data type and NA counts")
  print(tibble(
    column_name = names(type_of_data),
    datatype = type_of_data,
    na_counts = na_counts
  ))
  
  message("member_casual column: unique values")
  unique_member_casual <- unique(df$member_casual)
  print(unique_member_casual)
  
}