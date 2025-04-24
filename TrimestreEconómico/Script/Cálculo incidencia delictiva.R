###############################################################################
###############################################################################
####################### ENSAYO TRIMESTRE ECONÓMICO ############################
###############################################################################
###############################################################################

############################### Librerías ####################################
library(pacman)
p_load(tidyverse, ggthemes, data.table, readr, purrr, httr, fs, zip)

############################### Delitos ########################################

# Definir rutas
ruta_delitos <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/TrimestreEconómico/Datos/SESNSP/IDM_NM_ene25.csv"
ruta_destino <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/TrimestreEconómico/Datos/Datos limpios"

# Crear la carpeta si no existe
if (!dir.exists(ruta_destino)) dir.create(ruta_destino, recursive = TRUE)

############################### Cargar y limpiar delitos ###############################

# Cargar el archivo
delitos <- fread(ruta_delitos, encoding = "Latin-1")

# Asegurar que las claves sean caracteres con ceros a la izquierda
delitos <- delitos %>%
  mutate(
    Clave_Ent = str_pad(Clave_Ent, width = 2, side = "left", pad = "0"),
    Clave_Mun = str_pad(`Cve. Municipio`, width = 5, side = "left", pad = "0")
  ) %>%
  select(-`Cve. Municipio`)  # Eliminar la columna original

# Reordenar `Clave_Mun`
delitos <- delitos %>%
  relocate(Clave_Mun, .after = 3)

# Filtrar solo violencia familiar
violencia_familiar <- delitos %>% 
  filter(`Tipo de delito` == "Violencia familiar")

# Convertir meses a formato tidy
violencia_familiar <- violencia_familiar %>% 
  pivot_longer(cols = Enero:Diciembre, names_to = "Mes", values_to = "Delitos")

# Diccionario de meses
meses_dict <- c("Enero" = "01", "Febrero" = "02", "Marzo" = "03",
                "Abril" = "04", "Mayo" = "05", "Junio" = "06",
                "Julio" = "07", "Agosto" = "08", "Septiembre" = "09",
                "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")

# Convertir meses a formato numérico y crear fecha
violencia_familiar <- violencia_familiar %>%
  mutate(Mes_Num = meses_dict[Mes],
         Fecha = paste0(Mes_Num, "-", Año)) %>%
  select(-Mes_Num)

############################### Cargar y limpiar censo ###############################

# Ruta local del censo
ruta_censo <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/TrimestreEconómico/Datos/INEGI/ITER_NAL_2020_csv.zip"
unzip_dir <- "iter_datos"

# Descomprimir si no está ya extraído
if (!dir.exists(unzip_dir)) {
  unzip(ruta_censo, exdir = unzip_dir)
}

# Listar archivos extraídos
files <- list.files(unzip_dir, full.names = TRUE, pattern = "\\.csv$")

# Leer el censo en UTF-8
raw_lines <- readLines(files[1], encoding = "ISO-8859-1")
writeLines(raw_lines, "temp_utf8.csv", useBytes = TRUE)
censo_iter <- fread("temp_utf8.csv", encoding = "UTF-8")

# Corregir claves de entidad y municipio
censo_iter <- censo_iter %>%
  mutate(
    ENTIDAD = str_pad(ENTIDAD, width = 2, side = "left", pad = "0"),
    MUN = str_pad(MUN, width = 3, side = "left", pad = "0")
  )

# Filtrar municipios válidos eliminando "Total del Municipio"
censo_iter <- censo_iter %>%
  mutate(NOM_LOC = str_trim(str_squish(NOM_LOC))) %>%
  filter(NOM_LOC != "Total del Municipio")

# Sumar POBTOT por municipio
censo_municipal <- censo_iter %>%
  group_by(ENTIDAD, NOM_ENT, MUN, NOM_MUN) %>%
  summarise(POBTOT = sum(POBTOT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Clave_Mun = paste0(ENTIDAD, MUN))

############################### Unir datos y calcular incidencia ###############################

# Unir delitos con censo
violencia_familiar <- violencia_familiar %>%
  left_join(censo_municipal %>% select(Clave_Mun, POBTOT), by = "Clave_Mun")

# Calcular incidencia delictiva por 100K habitantes
violencia_familiar <- violencia_familiar %>%
  mutate(Incidencia = (Delitos / POBTOT) * 100000)

# Reordenar columnas
violencia_familiar <- violencia_familiar %>%
  select(Fecha, Clave_Ent, Entidad, Clave_Mun, Municipio, Delitos, Incidencia, 
         `Bien jurídico afectado`, `Tipo de delito`, `Subtipo de delito`, Modalidad)

# Filtrar fechas hasta enero de 2025
violencia_familiar <- violencia_familiar %>%
  filter(as.Date(paste0("01-", Fecha), format = "%d-%m-%Y") < as.Date("2025-02-01"))

############################### Guardar base final ###############################

# Guardar en la carpeta de destino
ruta_guardado <- file.path(ruta_destino, "violencia_familiar_municipal.csv")
write_csv(violencia_familiar, ruta_guardado)

# Confirmar guardado
print(paste("✅ Base guardada en:", ruta_guardado))
