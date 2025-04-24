###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, data.table, readr, purrr, httr, fs, zip, readr,
       readxl, Hmisc)


############################ Carga ENOE 2024T4 ################################

# Definir la ruta donde están los archivos
ruta <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/TrimestreEconómico/Datos/ENOE"

# Obtener la lista de archivos que terminan en "24.csv" y contienen "T1"
archivos_t4_2024 <- list.files(path = ruta, pattern = "T1.*24\\.csv$", full.names = TRUE)

# Cargar cada archivo en un objeto separado en el entorno global
for (archivo in archivos_t4_2024) {
  nombre_variable <- gsub(".csv", "", basename(archivo))  # Remover extensión
  nombre_variable <- gsub("-", "_", nombre_variable)  # Reemplazar guiones si los hubiera
  print(paste("Cargando archivo:", archivo, "como", nombre_variable))  # Depuración
  assign(nombre_variable, readr::read_csv(archivo), envir = .GlobalEnv)  # Guardar en el Global Environment
}

############################## Homologación de claves ##########################

# Lista con las bases de datos
tablas <- list("COE1" = ENOE_COE1T124,
               "COE2" = ENOE_COE2T124,
               "HOG"  = ENOE_HOGT124,
               "SDEM" = ENOE_SDEMT124,
               "VIV"  = ENOE_VIVT124)

# Iterar sobre cada base de datos y modificar las columnas requeridas
for (col_filtro in names(tablas)) {
  df <- tablas[[col_filtro]]  # Extraer la base de datos
  
  # Formatear columnas si existen
  if ("ent" %in% colnames(df)) {
    df$ent <- sprintf("%02d", as.numeric(df$ent))  # 2 dígitos
  }
  if ("mun" %in% colnames(df)) {
    df$mun <- sprintf("%03d", as.numeric(df$mun))  # 3 dígitos
  }
  if ("con" %in% colnames(df)) {
    df$con <- sprintf("%05d", as.numeric(df$con))  # 5 dígitos
  }
  if ("v_sel" %in% colnames(df)) {
    df$v_sel <- sprintf("%02d", as.numeric(df$v_sel))  # 2 dígitos
  }
  if ("n_ren" %in% colnames(df)) {
    df$n_ren <- sprintf("%02d", as.numeric(df$n_ren))   # 2 dígitos
  }
  
  # Guardar los cambios en la tabla original
  tablas[[col_filtro]] <- df
}

# Asignar las bases de datos modificadas a sus nombres originales
ENOE_COE1T124 <- tablas[["COE1"]]
ENOE_COE2T124 <- tablas[["COE2"]]
ENOE_HOGT124  <- tablas[["HOG"]]
ENOE_SDEMT124 <- tablas[["SDEM"]]
ENOE_VIVT124  <- tablas[["VIV"]]



################################# ETIQUETAS ###################################

mapeo_variables <- read_excel("GitHub/ServicioBecario/TrimestreEconómico/Datos/Mapeo Variables.xlsx")

# Lista de bases de datos
tablas <- list("COE1" = ENOE_COE1T124,
               "COE2" = ENOE_COE2T124,
               "HOG"  = ENOE_HOGT124,
               "SDEM" = ENOE_SDEMT124,
               "VIV"  = ENOE_VIVT124)

# Recorremos cada base de datos en la lista
for (col_filtro in names(tablas)) {
  df <- tablas[[col_filtro]]  # Extraer la base de datos
  
  # Verificar si la columna existe en `mapeo_variables`
  if (!(col_filtro %in% colnames(mapeo_variables))) {
    message(paste("Advertencia: No se encontró la columna", col_filtro, "en mapeo_variables"))
    next  # Saltar a la siguiente iteración si la columna no existe
  }
  
  # Filtrar `mapeo_variables` para la tabla actual
  mapeo_filtro <- mapeo_variables[mapeo_variables[[col_filtro]] == 1, ]
  
  # Verificar si hay variables en el filtro
  if (nrow(mapeo_filtro) == 0) {
    message(paste("Advertencia: No hay variables mapeadas para", col_filtro))
    next
  }
  
  # Aplicar las etiquetas de las variables a las columnas correspondientes
  for (i in seq_len(nrow(mapeo_filtro))) {
    var_name <- as.character(mapeo_filtro$Variable[i])  # Nombre de la columna
    var_desc <- as.character(mapeo_filtro$Descripción[i])  # Descripción
    
    if (var_name %in% colnames(df)) {
      label(df[[var_name]]) <- var_desc  # Asignar descripción como etiqueta
    }
  }
  
  # Guardar la base de datos modificada
  tablas[[col_filtro]] <- df
}

# Asignar las bases de datos modificadas a sus nombres originales
ENOE_COE1T124 <- tablas[["COE1"]]
ENOE_COE2T124 <- tablas[["COE2"]]
ENOE_HOGT124  <- tablas[["HOG"]]
ENOE_SDEMT124 <- tablas[["SDEM"]]
ENOE_VIVT124  <- tablas[["VIV"]]

############################### CONSOLIDACIÓN #################################

# Seleccionar bases relevantes
COE1 <- ENOE_COE1T124
COE2 <- ENOE_COE2T124
SDEM <- ENOE_SDEMT124

# Definir claves para hacer el join
claves_join <- c("tipo", "mes_cal", "cd_a", "ent", "con", "v_sel", "n_hog", "h_mud", "n_ren")

# Unir COE2 y SDEM a COE1 usando las claves definidas
ENOET124 <- merge(COE1, COE2, by = claves_join, all.x = TRUE)
ENOET124 <- merge(ENOET124, SDEM, by = claves_join, all.x = TRUE)

