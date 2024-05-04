# Caso práctico: análisis del servicio de bicicletas compartidas de la empresa Cyclistic

## Descripción
El repositorio contiene los recursos que fueron necesarios para el desarrollo de un caso práctico requerido en la instancia final del curso `Certificado profesional de Análisis de Datos de Google` dictado por Google en convenio con la plataforma educativa Coursera. Este proyecto aplica conocimientos prácticos de preparación, procesamiento y análisis de datos para una empresa ficticia llamada Cyclistic, el cual ofrece servicios de bicicletas compartidas en la ciudad de Chicago, Illinois, Estados Unidos.

La compañía en cuestión opera una flota de bicicletas georreferenciadas disponibles en estaciones que se encuentran distribuidas en toda la ciudad, permitiendo a los usuarios que hacen uso del servicio desbloquear bicicletas desde una estación y devolverlas en cualquier otra del sistema. Desde 2016, ha logrado atraer a un amplio segmento de consumidores mediante una estrategia de precios flexible, incluyendo pases de un solo viaje, pases de un día completo y membresías anuales.

En este sentido, la directora del área de marketing nos encomienda una tarea: analizar datos históricos de viajes de los usuarios con el fin de identificar tendencias y diferencias en los patrones de uso del servicio entre los clientes que cuentan con una membresía anual y los clientes ocasionales que compran pases de un solo viaje o de un día completo. 

Se espera que las conclusiones derivadas del análisis sirvan como insumo para el diseño de estrategias de marketing centradas principalmente en promover la conversión de clientes ocasionales en miembros anuales para garantizar el crecimiento futuro de la empresa.


## Fuente de datos
Los conjuntos de datos utilizados en este análisis son reales y están disponibles públicamente [aquí](https://divvy-tripdata.s3.amazonaws.com/index.html). Estos datos están bajo una [licencia](https://divvybikes.com/data-license-agreement) que permite su uso según los términos especificados.


## Preparación del entorno de trabajo
Si desea clonar y ejecutar este análisis, asegúrese de tener instalado [R](https://cran.r-project.org) y [RStudio](https://rstudio.com/products/rstudio/download/). Estas herramientas serán necesarias para iniciar sesión en RStudio a través de `Project_Cyclistic.Rproj` que se encuentra en el directorio raíz del proyecto y acceder al código de los scripts.


## Clonación del repositorio
Para clonar este proyecto, debe abrir una terminal o línea de comandos en su sistema operativo. Si está utilizando Windows, puede usar Git Bash o el símbolo del sistema. Ejecute los siguientes comandos de Git:
```bash
git clone https://github.com/javote94/project_cyclistic.git
cd Project_Cyclistic
```
Esto descargará el repositorio completo en su máquina local y cambiará al directorio del proyecto.


## Estructura del proyecto
Este proyecto está organizado en varias carpetas, además de incluir archivos importantes en el directorio raíz:
- `data/`: Almacena los datos descargados en `raw/csv/` y los transformados en `processed/csv/`. Ambas rutas contienen un archivo `.gitkeep` para persistir la estructura del proyecto en el repositorio remoto.
- `scripts/`: Contiene los scripts de R necesarios para procesamiento y análisis de datos, y renderización del reporte HTML.
- `results/`: Guarda gráficos y tablas generados por los scripts. Las subcarpetas `graphics/` y `tables/` incluyen `.gitkeep`.
- `reports/`: Incluye archivo R Markdown para la generación de reporte HTML.
- En el directorio raíz, encontrará `Project_Cyclistic.Rproj` para abrir el proyecto en RStudio y `index.html`, que es el informe final accesible a través de un navegador web.


## Descarga de datos 
Es fundamental descargar mediante el enlace proporcionado los conjuntos datos en formato csv y almacenarlos en la carpeta `data/raw/csv/` antes de proceder a ejecutar los scripts de R.


## Ejecución de scripts
Para llevar a cabo el análisis:
1. Asegúrese de haber descargado y colocado los conjuntos de datos necesarios en la carpeta `data/raw/csv/`, como se mencionó anteriormente.
2. Inicie RStudio a través de `Project_Cyclistic.Rproj` y verifique que el directorio de trabajo esté configurado para el directorio raíz del proyecto.
3. Ejecute `master_script.R` ubicado en la carpeta `scripts/`. Este script coordina todos los scripts secundarios para procesar los datos y generar las visualizaciones y tablas estadísticas requeridas.


## Contribuciones
Si estás interesado en contribuir a este proyecto, estamos abiertos a pull requests o puedes [abrir un issue](https://github.com/javote94/project_cyclistic/issues) para discutir posibles cambios. Valoramos especialmente las contribuciones relacionadas con análisis, documentación y mejoras en el código.


## Agradecimientos
Mis agradecimientos a Google y Coursera por proporcionar los datos y el contexto educativo para este análisis.

