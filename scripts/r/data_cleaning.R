
delete_duplicate_rows_by_id <- function(df) {
  # Eliminar filas que tienen valores duplicados en la columna ride_id (se mantiene la primera ocurrencia)
  initial_count <- nrow(df)
  df <- df %>%
    distinct(ride_id, .keep_all = TRUE)
  message(glue("Duplicated rows removed: {initial_count - nrow(df)}"))
  return(df)
}

remove_extra_spaces_in_strings <- function(df) {
  # Remover espacios extras al inicio, al final y entre las palabras de la cadena de texto en todas las columnas de tipo caracter
  return(
    df %>%
      mutate(across(where(is.character), ~ str_squish(str_trim(.))))
         )
}

clean_coordinates <- function(df) {
  # Descartar viajes con valor 0 (cero) o nulo (NA) en las columnas de coordenadas
  initial_count <- nrow(df)
  df <- df %>%
    filter(
      !is.na(start_lat) & !is.na(start_lng) & !is.na(end_lat) & !is.na(end_lng) &
        start_lat != 0 & start_lng != 0 & end_lat != 0 & end_lng != 0
    )
  message(glue("Coordinate cleaning -> number of rows removed: {initial_count - nrow(df)}"))
  return(df)
}

clean_travel_dates <- function(df) {
  # Descartar viajes donde la fecha de inicio sea igual o posterior a la fecha de finalización del recorrido
  initial_count <- nrow(df)
  df <- df %>%
    filter(started_at < ended_at)
  message(glue("Date cleaning -> number of rows removed: {initial_count - nrow(df)}"))
  return(df)
}

add_column_trip_distance <- function(df) {
  
  df <- df %>%
    mutate(
      trip_distance_meters = distHaversine(cbind(start_lng, start_lat), cbind(end_lng, end_lat)),
      trip_distance_meters = round(trip_distance_meters, digits = 0)
      )
  
  return(df)
}

add_column_trip_duration <- function(df) {

  df <- df %>%
    mutate(
      trip_duration_mins = as.numeric(difftime(ended_at, started_at, units = "mins")),
      trip_duration_mins = round(trip_duration_mins, digits = 1)
      )
  return(df)

}

add_column_speed_kmh <- function(df) {
  df <- df %>%
    mutate(
      speed_kmh = round((trip_distance_meters / 1000) / (trip_duration_mins / 60), 1)
    )
  return(df)
}

delete_short_trips <- function(df) {
  # Descartar viajes que duraron 1 minuto o menos
  if ("trip_duration_mins" %in% colnames(df)) {
    initial_count <- nrow(df)
    df <- df %>%
      filter(trip_duration_mins > 1.0)
    message(glue("Extremely short trips (one minute or less) -> number of rows removed: {initial_count - nrow(df)}"))
  } else {
    message("Column 'trip_duration_mins' does not exist in the dataframe.")
  }
  return(df)
}

delete_redundant_trips <- function(df) {
  
  initial_count <- nrow(df)
  # Descartar viajes que comienzan y terminan en la misma estación con una duración menor a dos minutos
  if ("trip_duration_mins" %in% colnames(df)) {
    df <- df %>%
      filter(
        !(start_station_name == end_station_name & 
            start_station_id == end_station_id & 
            trip_duration_mins < 2.0 &
            !is.na(start_station_name) & !is.na(start_station_id) &
            !is.na(end_station_name) & !is.na(end_station_id))
      )
  } else {
    message("Column 'trip_duration_mins' does not exist in the dataframe.")
  }
  
  # Descartar viajes que comienzan y terminan en la misma estación con un recorrido mayor a diez metros
  if ("trip_distance_meters" %in% colnames(df)) {
    df <- df %>%
      filter(
        !(start_station_name == end_station_name & 
            start_station_id == end_station_id & 
            trip_distance_meters > 10 &
            !is.na(start_station_name) & !is.na(start_station_id) &
            !is.na(end_station_name) & !is.na(end_station_id))
      )
  } else {
    message("Column 'trip_distance_meters' does not exist in the dataframe.")
  }
  message(glue("Redundant and incoherent trips -> number of rows removed: {initial_count - nrow(df)}"))
  return(df)
}

delete_atypical_trips <- function(df) {
  
  # Descartar viajes atípicos en velocidad (se excluye el decil más alto del subconjunto de valores atípicos)
  if ("speed_kmh" %in% colnames(df)) {
    
    initial_count <- nrow(df)
    
    # Identificar y aislar los valores atípicos superiores en un dataframe nuevo
    upper_limit <- quantile(df$speed_kmh, .75) + 1.5*IQR(df$speed_kmh)           # Rango intercuartil
    df_outliers <- df %>%
      filter(speed_kmh > upper_limit)
    
    # Aplicar la distribución decílica de los valores atípicos para identificar los que están en el decil 10
    df_outliers <- df_outliers %>%
      mutate(decile_outlier_speed_kmh = ntile(speed_kmh, 10))
    
    # Chequeo
    df_outliers %>%
      group_by(decile_outlier_speed_kmh) %>%
      summarise(
        count = n(),
        min_speed = min(speed_kmh, na.rm = TRUE), 
        max_speed = max(speed_kmh, na.rm = TRUE)
      )
    
    # Separar los registros con valores atípicos que están en el decil 10 en un df nuevo
    df_outliers_to_remove <- df_outliers %>%
      filter(decile_outlier_speed_kmh == 10)
    
    # Excluir registros del df original utilizando el campo ride_id.
    df <- df %>%
      filter(!ride_id %in% df_outliers_to_remove$ride_id)
    
    rm(df_outliers, df_outliers_to_remove)
    
    message(glue("Travel with atypical speed -> number of rows removed: {initial_count - nrow(df)}"))
    
  } else {
    message("Column 'speed_kmh' does not exist in the dataframe.")
  }
  return(df)
}

add_column_route <- function(df) {
  
  df <- df %>%
    mutate(route = paste(start_station_name, "to", end_station_name))
  
  return(df)
}

add_column_month_year <- function(df) {
  # Asegurar que 'started_at' está en df y es de tipo POSIXct
  if(!'started_at' %in% names(df) || !is.POSIXt(df$started_at)) {
    stop("'started_at' debe estar en df y ser de tipo POSIXct")
  }
  
  # Extraer el año y mes mínimo y máximo
  min_date <- floor_date(min(df$started_at), "month")
  max_date <- ceiling_date(max(df$started_at), "month") - days(1) # Ajuste para incluir el último mes
  
  # Crear secuencia de fechas desde el mínimo hasta el máximo por mes
  month_seq <- seq(min_date, max_date, by = "month")
  
  # Formatear como año-mes
  month_seq <- format(month_seq, "%Y-%m")
  
  # Crear la columna month_year y convertirla en factor con los niveles generados
  df <- df %>%
    mutate(
      trip_month_year = format(started_at, "%Y-%m"),
      trip_month_year = factor(trip_month_year, levels = month_seq, ordered = TRUE)
    )
  
  return(df)
}

add_column_day_of_week <- function(df) {
  
  df <- df %>%
    mutate(
      # Extrae el día de la semana como un número con lunes=1, domingo=7
      day_of_week_num = wday(started_at, week_start = 1),
      # Mapea el número al nombre completo del día en inglés
      day_of_week = case_when(
        day_of_week_num == 1 ~ "Monday",
        day_of_week_num == 2 ~ "Tuesday",
        day_of_week_num == 3 ~ "Wednesday",
        day_of_week_num == 4 ~ "Thursday",
        day_of_week_num == 5 ~ "Friday",
        day_of_week_num == 6 ~ "Saturday",
        day_of_week_num == 7 ~ "Sunday"
      ),
      # Convierte la columna en un factor con los días de la semana ordenados desde lunes a domingo
      day_of_week = factor(day_of_week,
                           levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    ) %>%
    select(-day_of_week_num) # Elimina la columna 'day_of_week_num'
  
  return(df)
}

add_column_hour <- function(df) {
  if(!'started_at' %in% names(df) || !is.POSIXt(df$started_at)) {
    stop("'started_at' debe estar en df y ser de tipo POSIXct")
  }
  
  # Extraer la hora de 'started_at' y crear la nueva columna 'hour'
  df <- df %>%
    mutate(trip_hour = hour(started_at))
  
  return(df)
}

add_column_decile_trip_distance <- function(df) {
  df <- df %>%
    mutate(decile_trip_distance = ntile(trip_distance_meters, 10))
  return(df)
}

add_column_decile_trip_duration <- function(df) {
  df <- df %>%
    mutate(decile_trip_duration = ntile(trip_duration_mins, 10))
  return(df)
}

delete_useless_columns <- function(df) {
  
  cols_to_remove <- c("start_station_id",
                      "end_station_id",
                      "started_at",
                      "ended_at")
  
  df <- df %>% select(-all_of(cols_to_remove))
  
  return(df)
}





