# Lista de paquetes a instalar y cargar
packages <- c("tidyverse",
              "skimr",
              "here",
              "janitor", 
              "glue",
              "geosphere",
              "openxlsx",
              "ggplot2"
              )

# Función para instalar y cargar paquetes si no están ya instalados
install_if_needed <- function(package) {
  if (!require(package, character.only = TRUE, quietly = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# Aplicar la función a cada paquete
invisible(sapply(packages, install_if_needed))
