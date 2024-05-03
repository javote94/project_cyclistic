# Cargar el paquete necesario
library(rmarkdown)

# Renderizar el documento R Markdown especificando la ubicación de salida correcta
render("reports/Report_v01.Rmd", output_file = "../index.html")


