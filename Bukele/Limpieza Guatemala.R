library("pacman")
library(dplyr)
library(stringr)
library(lubridate)
p_load("tidyverse", "memisc", "haven", "readxl")

########### Cargar bases de datos de detenidos ###########

# 2021
#URL del archivo CSV
url_2021 <- "

path = file.path("C:/", "Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/", "Detenidos2021.sav")
Detenidos2021 = read_sav(path)

# 2022
Detenidos2022 <- read_excel("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/Detenidos2022.xlsx")

# 2023
Detenidos2023 <- read_excel("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/Detenidos2023.xlsx")

# 2024
Detenidos2024 <- read_excel("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/Detenidos1T2024.xlsx")

# Población
Censo2018 <- read_excel("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/Censo2018.xlsx", 
                        sheet = "A1_2", skip = 4)

################### Pegar bases #######################

# Función para convertir todas las variables a character y agregar el año
convertir_y_agregar_año <- function(df, año) {
  df %>%
    mutate(across(where(is.numeric), as.character)) %>%  # Convierte todas las numéricas a character
    mutate(Año = año)  # Agrega la columna de año
}

# Aplicar la función a cada base de datos
Detenidos2021 <- convertir_y_agregar_año(Detenidos2021, 2021)
Detenidos2022 <- convertir_y_agregar_año(Detenidos2022, 2022)
Detenidos2023 <- convertir_y_agregar_año(Detenidos2023, 2023)
Detenidos2024 <- convertir_y_agregar_año(Detenidos2024, 2024)


######## Homologación códigos ##########
diccionario <- read_excel("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Bases Guatemala/Diccionario Datos.xlsx")

#Renombrar las columnas para que sean más fáciles de manejar
colnames(diccionario) <- c("variable", "codigo", "etiqueta")

#Convertir código a texto si es numérico
diccionario <- diccionario %>%
  mutate(codigo = as.character(codigo))

#Filtrar el diccionario con las variables que queremos homologar
diccionario_mes <- diccionario %>%
  filter(variable == "MES DE OCURRENCIA") %>%
  dplyr::select(codigo, etiqueta)

diccionario_depto <- diccionario %>% 
  filter(variable == "DEPARTAMENTO DE OCURRENCIA") %>% 
  dplyr::select(codigo, etiqueta)

diccionario_mupio <- diccionario %>% 
  filter(variable == "MUNICIPIO DE OCURRENCIA") %>% 
  dplyr::select(codigo, etiqueta)

diccionario_delito <- diccionario %>% 
  filter(variable == "CAUSA COMETIDA") %>% 
  dplyr::select(codigo, etiqueta)

# Lista de variables a homologar
variables_homologar <- c("DIA DE OCURRENCIA", "HORA DE OCURRENCIA", "GRUPO DE CADA 6 HORAS",
  "GRUPO DE HORA MAÑANA,TARDE Y NOCHE",
  "ZONA DE OCURRENCIA", "SEXO", "EDAD SIMPLE",
  "GRUPO DE EDAD QUINQUENAL (MENOR 15 HASTA 60 Y MÁS)", 
  "EDADES QUINQUENALES (A PARTIR DE 0 HASTA 80 Y MÁS)",
  "GRUPO DE EDAD QUINQUENAL (MENOR 15 HASTA 80 Y MÁS)", "GRUPO DE CAUSAS"
)

# Generar diccionarios de homologación para todas las variables
diccionarios <- lapply(variables_homologar, function(var) {
  diccionario %>% 
    filter(variable == var) %>% 
    dplyr::select(codigo, etiqueta)
})

# Asignar nombres a cada diccionario según la variable
names(diccionarios) <- gsub(" ", "_", tolower(variables_homologar))  # Convertir nombres a formato legible en R

# Desempaquetar diccionarios en variables individuales (opcional)
list2env(diccionarios, envir = .GlobalEnv)


####### HOMOLOGAR DETENIDOS 2021 CON LAS DEMÁS BASES DE DATOS #######

#Añadiendo un cero adicional para que lea el código de los municpios
Detenidos2021 <- Detenidos2021 %>%
  mutate(mupio_ocu = str_pad(mupio_ocu, width = 4, side = "left", pad = "0"))

# Unir Detenidos2021 con los diccionarios usando left_join()
Detenidos2021 <- Detenidos2021 %>%
  mutate(
    mes_ocu = as.character(mes_ocu),
    depto_ocu = as.character(depto_ocu),
    mupio_ocu = as.character(mupio_ocu),
    delito_com = as.character(delito_com)
  ) %>%
  left_join(diccionario_mes, by = c("mes_ocu" = "codigo")) %>%
  left_join(diccionario_depto, by = c("depto_ocu" = "codigo")) %>%
  left_join(diccionario_mupio, by = c("mupio_ocu" = "codigo")) %>%
  left_join(diccionario_delito, by = c("delito_com" = "codigo")) %>% 
  left_join(`edades_quinquenales_(a_partir_de_0_hasta_80_y_más)`, by = c("edad_quinquenales" = "codigo")) %>% 
  left_join(grupo_de_cada_6_horas, by = c("g_hora" = "codigo")) %>% 
  left_join(grupo_de_causas, by = c ("g_delitos" = "codigo")) %>% 
  left_join(`grupo_de_edad_quinquenal_(menor_15_hasta_60_y_más)`, by = c("g_edad_60ymás" = "codigo")) %>% 
  left_join(`grupo_de_edad_quinquenal_(menor_15_hasta_80_y_más)`, by = c("g_edad_80ymás" = "codigo")) %>% 
  left_join(`grupo_de_hora_mañana,tarde_y_noche`, by = c("g_hora_mañ.tar.noch" = "codigo")) %>% 
  left_join(sexo, by = c("sexo_per" = "codigo")) %>% 
  left_join(dia_de_ocurrencia, by = c("día_ocu" = "codigo"))

# Reemplazar los valores originales por las etiquetas homologadas
Detenidos2021 <- Detenidos2021 %>%
  mutate(
    mes_ocu = etiqueta.x,
    depto_ocu = etiqueta.y,
    mupio_ocu = etiqueta.x.x,
    delito_com = etiqueta.y.y,
    edad_quinquenales = etiqueta.x.x.x,
    g_hora = etiqueta.y.y.y,
    g_delitos = etiqueta.x.x.x.x,
    g_edad_60ymás = etiqueta.y.y.y.y,
    g_edad_80ymás= etiqueta.x.x.x.x.x,
    g_hora_mañ.tar.noch = etiqueta.y.y.y.y.y,
    sexo_per = etiqueta.x.x.x.x.x.x,
    día_ocu = etiqueta.y.y.y.y.y.y
  ) %>%
  dplyr::select(-starts_with("etiqueta"))  # Eliminar columnas extra generadas en el join


#Unir todas las bases a la total
Detenidos_total <- bind_rows(
  Detenidos2021 %>% mutate(Año = 2021),
  Detenidos2022 %>% mutate(Año = 2022),
  Detenidos2023 %>% mutate(Año = 2023),
  Detenidos2024 %>% mutate(Año = 2024)
)


####GENERAR DOS BASES: SUBTIPO Y GRUPOS DE DELITOS##### 
#Arreglar nombres de subtipos   
unique(Detenidos_total$delito_com)

Detenidos_total <- Detenidos_total %>%
  mutate(delito_com = dplyr::recode(delito_com,
                            "Contra el patrimonio" = "Contra el Patrimonio",
                            "Extorsión y chantaje" = "Extorsión",
                            "Contra la libertad" = "Contra la Libertad",
                            "Otras causas (motivo) (Código interno)" = "Otras causas"))

unique(Detenidos_total$delito_com)


#Arreglar nombres de grupos 
unique(Detenidos_total$g_delitos)
Detenidos_total <- Detenidos_total %>%
  mutate(g_delitos = dplyr::recode(g_delitos,
                                   "Extorsión y chantaje" = "Extorsión",
                                   "Contra el patrimonio" = "Contra el Patrimonio",
                                   "Contra la libertad" = "Contra la Libertad"))
unique(Detenidos_total$g_delitos)


##CREAR SUBTIPO DE DELITO##
#Colapsar por año, mes y delitos
delitos_subtipo <- Detenidos_total %>%
  group_by(año_ocu, mes_ocu, delito_com) %>%
  summarise(total_casos = n(), .groups = "drop")

# Agregar la columna de incidencia
delitos_subtipo <- delitos_subtipo %>%
  mutate(incidencia = total_casos / 18120000 * 100000) #La población de Guatemala es 18120000 

#Cambiar fecha a formato yyyy-mm-dd
meses_dict <- c("Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04",
                "Mayo" = "05", "Junio" = "06", "Julio" = "07", "Agosto" = "08",
                "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")

delitos_subtipo <- delitos_subtipo %>%
  mutate(fecha = str_c(meses_dict[mes_ocu], "-", año_ocu)) %>%
  dplyr::select(fecha, everything())

delitos_subtipo <- delitos_subtipo %>%
  mutate(fecha = as.Date(paste0(año_ocu, "-", meses_dict[mes_ocu], "-01")))



#CREAR GRUPO DE DELITOS#
#Colapsar por año, mes y delitos
delitos_grupo <- Detenidos_total %>%
  group_by(año_ocu, mes_ocu, g_delitos) %>% 
  summarise(total_casos = n(), .groups = "drop")
  

# Agregar la columna de incidencia
delitos_grupo <- delitos_grupo %>%
  mutate(incidencia = total_casos / 18120000 * 100000) #La población de Guatemala es 18120000 

#Cambiar fecha a formato yyyy-mm-dd
meses_dict <- c("Enero" = "01", "Febrero" = "02", "Marzo" = "03", "Abril" = "04",
                "Mayo" = "05", "Junio" = "06", "Julio" = "07", "Agosto" = "08",
                "Septiembre" = "09", "Octubre" = "10", "Noviembre" = "11", "Diciembre" = "12")

delitos_grupo <- delitos_grupo %>%
  mutate(fecha = str_c(meses_dict[mes_ocu], "-", año_ocu)) %>%
  dplyr::select(fecha, everything())

delitos_grupo <- delitos_grupo %>%
  mutate(fecha = as.Date(paste0(año_ocu, "-", meses_dict[mes_ocu], "-01")))



############################# GRÁFICAS ########################################


####GRÁFICAS DE GUATEMALA####
###POR SUBTIPO DE DELITO###
#Definir la carpeta de salida para las imágenes
ruta_carpeta <- "C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Img_Delitos_Guatemala"

# Crear la carpeta si no existe
if (!dir.exists(ruta_carpeta)) {
  dir.create(ruta_carpeta)
}

# Definir la fecha de la línea de tratamiento
fecha_tratamiento <- as.Date("2022-03-01")

# Generar y guardar una gráfica por cada subtipo de delito
delitos_subtipo %>%
  split(.$delito_com) %>%  # Separar la base por subtipo de delito
  walk(function(df) {
    
    # Extraer el nombre del delito para el título y el archivo
    nombre_delito <- unique(df$delito_com)
    nombre_archivo <- paste0(ruta_carpeta, "/", gsub(" ", "_", nombre_delito), ".png")
    
    # Calcular la media de incidencia antes y después de 2022
    media_antes_2022 <- mean(df$incidencia[df$fecha < fecha_tratamiento], na.rm = TRUE)
    media_despues_2022 <- mean(df$incidencia[df$fecha >= fecha_tratamiento], na.rm = TRUE)
    
    # Encontrar el mínimo y máximo de la fecha para cada período
    fecha_min <- min(df$fecha, na.rm = TRUE)
    fecha_max <- max(df$fecha, na.rm = TRUE)
    
    # Crear la gráfica con fondo blanco y color #3943B7
    p <- ggplot(df, aes(x = fecha, y = incidencia)) +
      geom_line(color = "#3943B7", linewidth = 1) +  # Línea de incidencia en color solicitado
      geom_vline(xintercept = as.numeric(fecha_tratamiento), linetype = "dashed", color = "black", linewidth = 1) + # Línea punteada negra
      geom_segment(aes(x = fecha_min, xend = fecha_tratamiento, y = media_antes_2022, yend = media_antes_2022), 
                   color = "red", linetype = "dotted", linewidth = 1) +  # Línea roja SOLO antes de 2022-03-01
      geom_segment(aes(x = fecha_tratamiento, xend = fecha_max, y = media_despues_2022, yend = media_despues_2022), 
                   color = "blue", linetype = "dotted", linewidth = 1) +  # Línea azul SOLO después de 2022-03-01
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "white", color = "white"), # Fondo blanco
        plot.background = element_rect(fill = "white", color = "white")   # Fondo blanco general
      ) +
      labs(
        title = paste("Incidencia Delictiva en Guatemala, Subtipo:", nombre_delito),
        x = "Fecha", 
        y = "Delitos por cada 100,000 habitantes"
      )
    
    # Guardar la imagen
    ggsave(filename = nombre_archivo, plot = p, width = 10, height = 6, dpi = 300)
  })



###POR GRUPO DE DELITO###
# Definir la carpeta de salida para las imágenes
ruta_carpeta <- "C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/Img_GRUPO_Delitos_Guatemala"

# Crear la carpeta si no existe
if (!dir.exists(ruta_carpeta)) {
  dir.create(ruta_carpeta)
}

# Definir la fecha de la línea de tratamiento
fecha_tratamiento <- as.Date("2022-03-01")

# Generar y guardar una gráfica por cada subtipo de delito
delitos_grupo %>%
  split(.$g_delitos) %>%  # Separar la base por subtipo de delito
  walk(function(df) {
    
    # Extraer el nombre del delito para el título y el archivo
    nombre_delito <- unique(df$g_delitos)
    nombre_archivo <- paste0(ruta_carpeta, "/", gsub(" ", "_", nombre_delito), ".png")
    
    # Calcular la media de incidencia antes y después de 2022
    media_antes_2022 <- mean(df$incidencia[df$fecha < fecha_tratamiento], na.rm = TRUE)
    media_despues_2022 <- mean(df$incidencia[df$fecha >= fecha_tratamiento], na.rm = TRUE)
    
    # Encontrar el mínimo y máximo de la fecha para cada período
    fecha_min <- min(df$fecha, na.rm = TRUE)
    fecha_max <- max(df$fecha, na.rm = TRUE)
    
    # Crear la gráfica con fondo blanco y color #3943B7
    p <- ggplot(df, aes(x = fecha, y = incidencia)) +
      geom_line(color = "#3943B7", linewidth = 1) +  # Línea de incidencia en color solicitado
      geom_vline(xintercept = as.numeric(fecha_tratamiento), linetype = "dashed", color = "black", linewidth = 1) + # Línea punteada negra
      geom_segment(aes(x = fecha_min, xend = fecha_tratamiento, y = media_antes_2022, yend = media_antes_2022), 
                   color = "red", linetype = "dotted", linewidth = 1) +  # Línea roja SOLO antes de 2022-03-01
      geom_segment(aes(x = fecha_tratamiento, xend = fecha_max, y = media_despues_2022, yend = media_despues_2022), 
                   color = "blue", linetype = "dotted", linewidth = 1) +  # Línea azul SOLO después de 2022-03-01
      theme_minimal(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "white", color = "white"), # Fondo blanco
        plot.background = element_rect(fill = "white", color = "white")   # Fondo blanco general
      ) +
      labs(
        title = paste("Incidencia Delictiva en Guatemala, Grupo: ", nombre_delito),
        x = "Fecha", 
        y = "Delitos por cada 100,000 habitantes"
      )
    
    # Guardar la imagen
    ggsave(filename = nombre_archivo, plot = p, width = 10, height = 6, dpi = 300)
  })



