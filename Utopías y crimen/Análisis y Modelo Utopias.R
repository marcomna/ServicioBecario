###############################################################################
###############################################################################
################################ UTOPÍAS #####################################
###############################################################################
###############################################################################
###############################################################################

########################### CARGA DE DATOS ###################################

library(pacman)  # Asegúrate de tener instalada la librería pacman
p_load(readr, tidyverse, sf, purrr)

###### CARPETAS DE INVESTIGACIÓN

url_carpetas <- "https://archivo.datos.cdmx.gob.mx/FGJ/carpetas/carpetasFGJ_acumulado_2025_01.csv"

# Leer el CSV directamente desde la URL y guardarlo en el objeto 'carpetas'
carpetas <- read_csv(url_carpetas)

# Mostrar las primeras filas para confirmar que se cargaron correctamente
head(carpetas)

# Quitamos algunas columnas innecesarias
carpetas <- carpetas %>% 
  select(-anio_inicio, -mes_inicio, -hora_inicio, -anio_hecho, -mes_hecho,
         -hora_hecho, -competencia, -fiscalia, -agencia, -unidad_investigacion,
         -colonia_hecho, -colonia_catalogo, -alcaldia_hecho, -alcaldia_catalogo,
         -municipio_hecho)

# Droppeamos observaciones en donde latitud o longitud sean NA
# Se pierden 101,207 observaciones, 4.8% de los datos -> es aceptable
carpetas <- carpetas %>% 
  filter(!is.na(latitud) | !is.na(longitud))

###### UTOPÍAS

# URL "raw" del archivo comprimido en GitHub para Utopías
utopias_zip_url <- "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Utop%C3%ADas%20y%20crimen/Capas/Utop%C3%ADas.zip"

# Descargamos el zip a un archivo temporal para utopías
temp_utopias <- tempfile(fileext = ".zip")
download.file(utopias_zip_url, temp_utopias, mode = "wb")

# Crear un directorio temporal específico para descomprimir utopías
unzip_dir_utopias <- file.path(tempdir(), "utopias")
dir.create(unzip_dir_utopias, showWarnings = FALSE)

# Descomprimir el zip de utopías
unzip(temp_utopias, exdir = unzip_dir_utopias)

# Buscar todos los archivos .shp en el directorio de utopías
shp_files_utopias <- list.files(unzip_dir_utopias, pattern = "\\.shp$", full.names = TRUE)

# Si hay más de uno, usar el primero
if(length(shp_files_utopias) > 1){
  message("Se encontraron múltiples archivos .shp en utopías. Se usará: ", shp_files_utopias[1])
}

# Leer el shapefile de utopías
utopias <- st_read(shp_files_utopias[1])

###### AGEBs

# URL "raw" del archivo comprimido en GitHub para AGEBS
agebs_zip_url <- "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Utop%C3%ADas%20y%20crimen/Capas/CDMX/conjunto_de_datos/09a.zip"

# Descargamos el zip a un archivo temporal para AGEBS
temp_agebs <- tempfile(fileext = ".zip")
download.file(agebs_zip_url, temp_agebs, mode = "wb")

# Crear un directorio temporal específico para descomprimir AGEBS
unzip_dir_agebs <- file.path(tempdir(), "agebs")
dir.create(unzip_dir_agebs, showWarnings = FALSE)

# Descomprimir el zip de AGEBS
unzip(temp_agebs, exdir = unzip_dir_agebs)

# Buscar todos los archivos .shp en el directorio de AGEBS
shp_files_agebs <- list.files(unzip_dir_agebs, pattern = "\\.shp$", full.names = TRUE)

# Si hay más de uno, usar el primero
if(length(shp_files_agebs) > 1){
  message("Se encontraron múltiples archivos .shp en AGEBS. Se usará: ", shp_files_agebs[1])
}

# Leer el shapefile de AGEBS
agebs <- st_read(shp_files_agebs[1])

# Mostrar un resumen del shapefile de AGEBS
print(agebs)

###### POBLACIÓN

# URL del archivo CSV en GitHub (raw)
url_csv <- "https://raw.githubusercontent.com/marcomna/ServicioBecario/refs/heads/main/Utop%C3%ADas%20y%20crimen/Datos/poblacion.csv"

# Cargar el CSV en un data frame
poblacion <- read_csv(url_csv)

# 1. En el data frame "poblacion", crea la columna CVEGEO concatenando MUN, LOC y AGEB
poblacion <- poblacion %>%
  mutate(CVEGEO = paste0(MUN, LOC, AGEB))

# 2. Sumar POBTOT por cada CVEGEO en "poblacion"
poblacion_sum <- poblacion %>%
  group_by(CVEGEO) %>%
  summarize(POBTOT = sum(POBTOT, na.rm = TRUE))

# 3. En el data frame "agebs", crea la columna CVEGEO concatenando CVE_MUN, CVE_LOC y CVE_AGEB
agebs <- agebs %>%
  mutate(CVEGEO = paste0(CVE_MUN, CVE_LOC, CVE_AGEB))

# 4. Realiza el join: une "agebs" con la suma de población usando CVEGEO
agebs <- agebs %>%
  left_join(poblacion_sum, by = "CVEGEO")

########################### LIMPIEZA DE DATOS ##################################

# Convertir 'carpetas' a objeto sf usando EPSG:4326 (grados) y luego transformarlo al CRS de agebs
carpetas_sf <- st_as_sf(carpetas, coords = c("longitud", "latitud"), crs = 4326) %>% 
  st_transform(st_crs(agebs))

# Realizar el join espacial para agregar los atributos de agebs (CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB y POBTOT)
carpetas_joined <- st_join(carpetas_sf, 
                           agebs %>% select(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB),
                           join = st_intersects, 
                           left = TRUE)

# Agregar además la información del polígono que contiene cada punto (geom_agebs)
matches <- st_within(carpetas_sf, agebs)
carpetas_joined$geom_agebs <- map(matches, function(idx) {
  if(length(idx) > 0) {
    st_geometry(agebs)[[idx[1]]]
  } else {
    NA
  }
})

