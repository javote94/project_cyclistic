
# Importar scripts ------------------------------------------------------------
source("scripts/r/setup_packages.R")
source("scripts/r/data_import.R")
source("scripts/r/data_verification.R")
source("scripts/r/data_cleaning.R")
source("scripts/r/data_analysis.R")
source("scripts/r/excel_script.R")
source("scripts/r/data_visualization.R")

# Importar datos csv ----------------------------------------------------------
df_list <- import_data_csv("data/raw/csv/")

# Verificar compatibilidad de los dataframes ----------------------------------
check_compatibility(df_list)

# Combinar los dataframes -----------------------------------------------------
df <- bind_rows(df_list)

rm(df_list)              #remover objeto

skim_without_charts(df)  #resumen estadístico
verify_data(df)          #resumen estadístico

# Exportar dataframe combinado antes de limpiar -------------------------------
write_csv(df, "data/processed/csv/combined_df.csv")

# Limpieza y transformación ---------------------------------------------------
df <- delete_duplicate_rows_by_id(df)
df <- remove_extra_spaces_in_strings(df)

df <- clean_coordinates(df)
df <- clean_travel_dates(df)

df <- add_column_trip_distance(df)
df <- add_column_trip_duration(df)
df <- add_column_speed_kmh(df)

df <- delete_short_trips(df)
df <- delete_redundant_trips(df)
df <- delete_atypical_trips(df)

df <- add_column_route(df)
df <- add_column_month_year(df)
df <- add_column_day_of_week(df)
df <- add_column_hour(df)

df <- add_column_decile_trip_distance(df)
df <- add_column_decile_trip_duration(df)

df_clean <- delete_useless_columns(df)
rm(df)              #remover objeto

# Exportar dataframe limpio antes del análisis --------------------------------
write_csv(df_clean, "data/processed/csv/clean_df.csv")

# Elaboración de cuadros estadísticos (análisis) ------------------------------
table_1 <- summary_monthly_distribution_trips(df_clean)
table_2 <- summary_trips_duration_by_client(df_clean)
table_3 <- summary_trips_distance_by_client(df_clean)
table_4 <- summary_trips_by_client_and_month(df_clean)
table_5 <- summary_trips_duration_by_client_and_month(df_clean)
table_6 <- summary_trips_by_client_and_day(df_clean)
table_7 <- summary_trips_duration_by_client_and_day(df_clean)
table_8 <- summary_trips_by_client_day_month(df_clean)
table_9 <- summary_trips_duration_by_client_day_month(df_clean)
table_10 <- summary_trips_avg_duration_by_client_and_day(df_clean)
table_11 <- summary_trips_by_client_and_hour(df_clean)
table_12 <- summary_trips_by_client_and_rideable(df_clean)

# Guardar tablas en Excel -----------------------------------------------------
file_path <- "results/tables/resultados_r.xlsx"
create_and_save_excel(file_path, table_1, table_2, table_3, table_4, table_5,
                      table_6, table_7, table_8, table_9, table_10, table_11,
                      table_12)

# Visualizaciones -------------------------------------------------------------
g1 <- table_1 %>%
  graph_bar_xmonth_ypercentage() +
  labs(
    x = "Year-Month",
    y = "% of total trips", 
    title = "Monthly distribution of trips in the data set. March 2023 to February 2024")


g2 <- table_4 %>%
  graph_bar_xmonth_ypercentage_by_client() +
  labs(x = "Year-Month",
       y = "% of total trips",
       fill = "Customer type",
       title = "Number of trips (in %) by type of client. March 2023 to February 2024")

g3 <- table_4 %>%
  graph_bar_xmonth_ynumber_by_client() +
  labs(x = "Year-Month",
       y = "Number of trips",
       fill = "Customer type",
       title = "Number of trips according to type of client. March 2023 to February 2024")


g4 <-  table_4 %>%
  graph_line_xmonth_yvariation_by_client() +
  labs(x = "Year-Month",
       y = "Percentage variation in trips (%)",
       colour = "Customer type",
       title = "Evolution(%) of the number of trips by type of customer. March 2023 to February 2024")

g5 <- table_5 %>%
  graph_bar_xmonth_ypercentage_by_client() +
  labs(x = "Year-Month",
       y = "% of accumulated travel hours",
       fill = "Customer type",
       title = "Accumulated travel hours (in %) by type of client. March 2023 to February 2024")


g6 <- table_5 %>%
  graph_line_xmonth_yvariation_by_client_v02() +
  labs(x = "Year-Month",
       y = "Percentage variation in travel hours (%)",
       colour = "Customer type",
       title = "Evolution of the number of accumulated travel hours by type of customer.March 2023 to February 2024")

g7 <- table_6 %>%
  graph_bar_xday_ypercentage_by_client() +
  labs(x = "Day of week",
       y = "% of total trips",
       fill = "Customer type",
       title = "Number of trips (%) by type of client and day of week")

g8 <- table_7 %>%
  graph_bar_xday_ypercentage_by_client() +
  labs(x = "Day of week",
       y = "% of accumulated travel hours",
       fill = "Customer type",
       title = "Accumulated travel hours (in %) by type of client and day of week")

g9 <- table_8 %>%
  graph_bar_xday_ypercentage_facetmonth_by_client() +
  labs(x = "Day of week",
       y = "% of total trips",
       fill = "Customer type",
       title = "Falta título")

g10 <- table_9 %>%
  graph_bar_xday_ypercentage_facetmonth_by_client() +
  labs(x = "Day of week",
       y = "% of accumulated travel hours",
       fill = "Customer type",
       title = "Falta título")

g11 <- table_10 %>%
  graph_bar_xday_yavgduration_by_client() +
  labs(x = "Day of week",
       y = "average trip duration (in minutes)",
       fill = "Customer type",
       title = "Average trip duration per day according to type of client")

g12 <- table_11 %>%
  graph_area_xhour_ypercentage_by_client() +
  labs(x = "Hour of Day",
       y = "Percentage of Total Trips",
       fill = "Type of Customer",
       title = "Hourly Distribution of Trips by Customer Type",
       subtitle = "Proportion of total trips for each hour of the day")
  

g13 <- df_clean %>%
  graph_density_xlatitude() +
  labs(title = "Start and end latitude distribution",
       x = "Latitude",
       y = "Density")

g14 <- df_clean %>%
  graph_density_xlength() +
  labs(title = "Start and end length distribution",
       x = "Length",
       y = "Density")


