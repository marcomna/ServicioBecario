###############################################################################
###############################################################################
################################ UTOPÍAS #####################################
###############################################################################
###############################################################################
###############################################################################

########################### CARGA DE DATOS ###################################

library(pacman) # Instalar primero si no está instalada
p_load(readr, tidyverse, sf)

###### CARPETAS DE INVESTIGACIÓN

url <- "https://archivo.datos.cdmx.gob.mx/FGJ/carpetas/carpetasFGJ_acumulado_2025_01.csv"

# Leer el CSV directamente desde la URL y guardarlo en el objeto 'carpetas'
carpetas <- read_csv(url)

# Mostrar las primeras filas para confirmar que se cargaron correctamente
head(carpetas)

# Quitamos algunas columnas innecesarias
carpetas <- carpetas %>% 
  select(-anio_inicio, -mes_inicio, -hora_inicio, -anio_hecho, -mes_hecho,
         -hora_hecho, -competencia, -fiscalia, -agencia, -unidad_investigacion,
         -colonia_hecho, -colonia_catalogo, -alcaldia_hecho, -alcaldia_catalogo,
         -municipio_hecho)

# Droppeamos observaciones en donde latitud o longitud sea NA
# Se pierden 101,207 observaciones, 4.8% de los datos -> es aceptable
carpetas <- carpetas %>% 
  filter(!is.na(latitud) | !is.na(longitud))

###### UTOPÍAS

# URL "raw" de tu archivo comprimido en GitHub
url_zip <- "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Utop%C3%ADas%20y%20crimen/Capas/Utop%C3%ADas.zip"

# Descargamos el zip a un directorio temporal
temp <- tempfile(fileext = ".zip")
download.file(url_zip, temp)

# Descomprimimos
unzip_dir <- tempdir()
unzip(temp, exdir = unzip_dir)

# Buscamos el .shp y lo leemos
shp_file <- list.files(unzip_dir, pattern = "\\.shp$", full.names = TRUE)
utopias <- st_read(shp_file)

# Verificamos que se haya cargado correctamente
print(utopias)

###### AGEBs

# URL del archivo zip en GitHub (versión raw)
url_zip <- "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Utop%C3%ADas%20y%20crimen/Capas/CDMX/conjunto_de_datos/09a.zip"

# Crear un archivo temporal para descargar el zip
temp_file <- tempfile(fileext = ".zip")
download.file(url_zip, temp_file, mode = "wb")

# Directorio temporal para descomprimir el zip
temp_dir <- tempdir()
unzip(temp_file, exdir = temp_dir)

# Buscar el archivo .shp dentro del directorio descomprimido
shp_file <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)

# Leer el shapefile (se asume que hay al menos un archivo .shp)
agebs <- st_read(shp_file[1])

# Mostrar un resumen del shapefile cargado
print(agebs)
