
graph_bar_xmonth_ypercentage <- function(table) {
  
  ggplot(table, aes(x = trip_month_year, y = percentage)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    scale_x_discrete(limits = rev) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
    geom_text(aes(label = sprintf("%.1f%%", percentage)), hjust = -0.1) +
    theme_bw()
}

graph_bar_xmonth_ypercentage_by_client <- function(table) {
  
  ggplot(table, aes(x = trip_month_year, y = percentage, fill = member_casual)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    scale_y_continuous(limits = c(0, 100)) +
    geom_text(
      aes(label = sprintf("%.0f%%", percentage)),
      vjust = -0.6,
      size = 3,
      position = position_dodge(width = 0.9)
      ) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "top"
    ) 
  
}

graph_bar_xmonth_ynumber_by_client <- function(table) {
  
  ggplot(table, aes(x = trip_month_year, y = number_trips, fill = member_casual)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    geom_text(
      aes(label = format(number_trips, big.mark = ".")),
      vjust = -0.6,
      size = 3,
      position = position_dodge(width = 0.9)
    ) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ".", scientific = FALSE),
                       limits = c(0, max(table$number_trips) * 1.1)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "top"
    )
  
}

graph_line_xmonth_yvariation_by_client <- function(table) {
  
  # Se cambia la estructura de la tabla y se agrega la columna de variación
  table <- table %>%
    mutate(
      trip_month_year = as.Date(paste(trip_month_year, "-01", sep=""))
    ) %>%
    arrange(member_casual, trip_month_year) %>%
    group_by(member_casual) %>%
    mutate(month_variation = (number_trips / lag(number_trips) - 1) * 100)
  
  # El valor NA del primer mes de la serie se pasa a cero
  table$month_variation[is.na(table$month_variation)] <- 0
  
  # Gráfico
  ggplot(table,
         aes(x = trip_month_year,
             y = month_variation,
             colour = member_casual,
             shape = member_casual)) +
    geom_line() + 
    geom_point(size = 3.2) +
    geom_text(
      aes(label = sprintf("%.0f%%", month_variation),
      vjust = ifelse(member_casual == "member", -0.7, 1.0), # Arriba para 'member', abajo para 'casual'
      hjust = ifelse(member_casual == "casual", 1.2, 0.1)), # Izquierda para 'casual', centrado para 'member'
      size = 3
    ) +
    scale_color_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    scale_shape_manual(values = c("member" = 19, "casual" = 17)) +
    guides(colour = guide_legend(), shape = FALSE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m") +
    scale_y_continuous(limits = c(-60, 160)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "top") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
    
  
  
}

graph_line_xmonth_yvariation_by_client_v02 <- function(table) {
  
  # Se cambia la estructura de la tabla y se agrega la columna de variación
  table <- table %>%
    mutate(
      trip_month_year = as.Date(paste(trip_month_year, "-01", sep=""))
    ) %>%
    arrange(member_casual, trip_month_year) %>%
    group_by(member_casual) %>%
    mutate(month_variation = (accum_trip_hours / lag(accum_trip_hours) - 1) * 100)
  
  # El valor NA del primer mes de la serie se pasa a cero
  table$month_variation[is.na(table$month_variation)] <- 0
  
  # Gráfico
  ggplot(table,
         aes(x = trip_month_year,
             y = month_variation,
             colour = member_casual,
             shape = member_casual)) +
    geom_line() + 
    geom_point(size = 3.2) +
    geom_text(
      aes(label = sprintf("%.0f%%", month_variation),
          vjust = ifelse(member_casual == "member", -0.7, 1.0), # Arriba para 'member', abajo para 'casual'
          hjust = ifelse(member_casual == "casual", 1.2, 0.1)), # Izquierda para 'casual', centrado para 'member'
      size = 3
    ) +
    scale_color_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    scale_shape_manual(values = c("member" = 19, "casual" = 17)) +
    guides(colour = guide_legend(), shape = FALSE) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%Y-%m") +
    scale_y_continuous(limits = c(-60, 250)) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5),
      legend.position = "top") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")
  
  
  
}

graph_bar_xday_ypercentage_by_client <- function(table) {
  
  ggplot(table, aes(x = day_of_week, y = percentage, fill = member_casual)) +
    geom_bar(stat = "identity") +
    geom_text(
      aes(label = sprintf("%.1f%%", percentage)),
      position = position_stack(vjust = 0.5),
      color = "black") +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32"))
  
}

graph_bar_xday_ypercentage_facetmonth_by_client <- function(table) {
  
  table$day_of_week <- factor(table$day_of_week,
                              levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"),
                              labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
  
  ggplot(table, aes(x = day_of_week, y = percentage, fill = member_casual)) +
    geom_bar(stat = "identity") +
    facet_wrap(~trip_month_year, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    geom_text(
      aes(label = sprintf("%.0f%%", percentage)),
      position = position_stack(vjust = 0.5),
      color = "black",
      size = 2.5) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1),
      # axis.title.y = element_blank(), # Oculta el título del eje y
      axis.text.y = element_blank(), # Oculta las etiquetas del eje y
      axis.ticks.y = element_blank(), # Oculta las marcas del eje y
      legend.position = "top")

}

graph_bar_xday_yavgduration_by_client <- function(table) {
  
  ggplot(table, aes(x = day_of_week, y = avg_duration_mins, fill = member_casual)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    geom_text(
      aes(label = sprintf("%.1f", avg_duration_mins)),
      vjust = -0.6,
      size = 3,
      position = position_dodge(width = 0.9)
    ) +
    theme_bw()
    
}

graph_area_xhour_ypercentage_by_client <- function(table) {
  
  ggplot(table, aes(x = trip_hour, y = percentage, fill = member_casual)) + 
    geom_area(position = 'identity', alpha = 0.5) +
    geom_line(aes(color = member_casual), size = 0.7) +
    scale_x_continuous(breaks = 0:23, minor_breaks = NULL) +
    scale_color_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32"),
                       guide = FALSE) +
    scale_fill_manual(values = c("member" = "#56B4E9", "casual" = "#FD9C32")) +
    theme_classic() +
    theme(legend.position = "top")
  
}

graph_density_xlatitude <- function(df) {
  
  ggplot(
    df %>%
      select(start_lat, end_lat) %>%
      pivot_longer(cols = c(start_lat, end_lat), names_to = "type", values_to = "latitude") %>%
      mutate(type = fct_rev(type)),
    aes(x = latitude)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~type, ncol = 1, scales = "free") +
    scale_x_continuous(
      breaks = function(x) pretty(x, n = 10),
      labels = function(x) sprintf("%.2f", x)
    ) +
    theme_bw()
}


graph_density_xlength <- function(df) {
  
  ggplot(
    df %>%
      select(start_lng, end_lng) %>%
      pivot_longer(cols = c(start_lng, end_lng), names_to = "type", values_to = "length") %>%
      mutate(type = fct_rev(type)),
    aes(x = length)) +
    geom_density(alpha = 0.5) +
    facet_wrap(~type, ncol = 1, scales = "free") +
    scale_x_continuous(
      breaks = function(x) pretty(x, n = 10),
      labels = function(x) sprintf("%.2f", x)
    ) +
    theme_bw()
}


  


