
summary_monthly_distribution_trips <- function(df_clean) {
  return(
    df_clean %>%
      group_by(trip_month_year) %>%
      summarise(number_trips = n(), .groups = 'drop') %>%
      mutate(percentage = round((number_trips / sum(number_trips)) * 100, digits = 1))
  )
}

summary_trips_duration_by_client <- function(df_clean) {
  return(
    df_clean %>%
      group_by(member_casual) %>%
      summarise(
        number_trips = n(),
        accum_trip_hours = round(sum(trip_duration_mins / 60), digits = 0),
        avg_trip_mins = round(mean(trip_duration_mins), digits = 1),
        .groups = 'drop'
      ) %>%
      mutate(
        percentage_trips = round(number_trips / sum(number_trips) * 100, digits = 1),
        percentage_hours = round(accum_trip_hours / sum(accum_trip_hours) * 100, digits = 1)
      ) %>%
      select(
        member_casual,
        number_trips,
        percentage_trips,
        accum_trip_hours,
        percentage_hours,
        avg_trip_mins
      ) %>%
      arrange(desc(number_trips)) %>%
      bind_rows(
        summarise(
          .,
          member_casual = "Total",
          number_trips = sum(number_trips),
          percentage_trips = 100,
          accum_trip_hours = sum(accum_trip_hours),
          percentage_hours = 100,
          avg_trip_mins = round((accum_trip_hours * 60) / number_trips, digits = 1)
        )
      )
  )
}

summary_trips_distance_by_client <- function(df_clean) {
  return(
    df_clean %>%
      group_by(member_casual) %>%
      summarise(
        number_trips = n(),
        accum_trip_km = round(sum(trip_distance_meters/1000), digits = 0),
        avg_trip_km = round(mean(trip_distance_meters/1000), digits = 2),
        .groups = 'drop'
      ) %>%
      mutate(
        percentage_trips = round(number_trips / sum(number_trips) * 100, digits = 1),
        percentage_km = round(accum_trip_km / sum(accum_trip_km) * 100, digits = 1)
      ) %>%
      select(member_casual,
             number_trips,
             percentage_trips,
             accum_trip_km,
             percentage_km,
             avg_trip_km) %>%
      arrange(desc(number_trips)) %>%
      bind_rows(
        summarise(
          .,
          member_casual = "Total",
          number_trips = sum(number_trips),
          percentage_trips = 100,
          accum_trip_km = sum(accum_trip_km),
          percentage_km = 100,
          avg_trip_km = round(accum_trip_km / number_trips, digits = 2)
        )
      )
  )
}

summary_trips_by_client_and_month <- function(df_clean) {
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(trip_month_year, member_casual) %>%
      summarise(number_trips = n()) %>%
      mutate(
        total_trips_month = sum(number_trips),
        percentage = round((number_trips / total_trips_month) * 100, digits = 1)
        )
  )
}

summary_trips_duration_by_client_and_month <- function(df_clean) {
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(trip_month_year, member_casual) %>%
      summarise(
        accum_trip_hours = round(sum(trip_duration_mins / 60), digits = 0),
        ) %>%
      mutate(
        total_hours_month = round(sum(accum_trip_hours), digits = 0),
        percentage = round((accum_trip_hours / total_hours_month) * 100, digits = 1)
      )
  )
}

summary_trips_by_client_and_day <- function(df_clean) {
  
  # Asegura que 'day_of_week' sea un factor ordenado cronológicamente
  df_clean$day_of_week <- factor(df_clean$day_of_week,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(day_of_week, member_casual) %>%
      summarise(number_trips = n()) %>%
      mutate(
        total_trips_day = sum(number_trips),
        percentage = round((number_trips / total_trips_day) * 100, digits = 1)
      )
  )
}

summary_trips_duration_by_client_and_day <- function(df_clean) {
  
  # Asegura que 'day_of_week' sea un factor ordenado cronológicamente
  df_clean$day_of_week <- factor(df_clean$day_of_week,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  return(
    df_clean %>%
      group_by(day_of_week, member_casual) %>%
      summarise(
        accum_trip_hours = round(sum(trip_duration_mins / 60), digits = 0)
        ) %>%
      mutate(
        total_hours_day = sum(accum_trip_hours),
        percentage = round((accum_trip_hours / total_hours_day) * 100, digits = 1)
      )
  )
}

summary_trips_avg_duration_by_client_and_day <- function(df_clean) {
  
  # Asegura que 'day_of_week' sea un factor ordenado cronológicamente
  df_clean$day_of_week <- factor(df_clean$day_of_week,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(day_of_week, member_casual) %>%
      summarise(
        avg_duration_mins = round(mean(trip_duration_mins), digits = 1),
      )
  )
}

summary_trips_by_client_day_month <- function(df_clean) {
  
  # Asegura que 'day_of_week' sea un factor ordenado cronológicamente
  df_clean$day_of_week <- factor(df_clean$day_of_week,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(trip_month_year, day_of_week, member_casual) %>%
      summarise(number_trips = n()) %>%
      mutate(
        total_trips_day = sum(number_trips),
        percentage = round((number_trips / total_trips_day) * 100, digits = 1)
      )
  )
  
}

summary_trips_duration_by_client_day_month <- function(df_clean) {
  
  # Asegura que 'day_of_week' sea un factor ordenado cronológicamente
  df_clean$day_of_week <- factor(df_clean$day_of_week,
                                 levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(trip_month_year, day_of_week, member_casual) %>%
      summarise(
        accum_trip_hours = round(sum(trip_duration_mins / 60), digits = 0)
        ) %>%
      mutate(
        total_hours_day = sum(accum_trip_hours),
        percentage = round((accum_trip_hours / total_hours_day) * 100, digits = 1)
      )
  )
  
}

summary_trips_by_client_and_hour <- function(df_clean) {
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(member_casual, trip_hour) %>%
      summarise(number_trips = n()) %>%
      mutate(
        total_trips = sum(number_trips),
        percentage = round((number_trips / total_trips) * 100, digits = 1)
        )
  )
}

summary_trips_by_client_and_rideable <- function(df_clean) {
  
  # Invierte el orden de 'member_casual'
  df_clean$member_casual <- factor(df_clean$member_casual,
                                   levels = c("member", "casual"))
  
  return(
    df_clean %>%
      group_by(member_casual, rideable_type) %>%
      summarise(number_trips = n()) %>%
      mutate(
        total_trips = sum(number_trips),
        percentage = round((number_trips / total_trips) * 100, digits = 1)
      )
      
  )
  
}


