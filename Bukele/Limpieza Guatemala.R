#################### ANÁLISIS DE INCIDENCIA DELICTIVA EN GUATEMALA ####################

#### LIBRERÍAS ####
library(pacman)
p_load(tidyverse, haven, readxl)

#### 1) Función para leer archivos desde GitHub ####
leer_github <- function(url, ext, ...) {
  tmp <- tempfile(fileext = ext)
  download.file(url, tmp, mode = "wb")
  if (ext == ".xlsx") read_excel(tmp, ...) 
  else if (ext == ".sav") read_sav(tmp) 
  else stop("Extensión no soportada.")
}

#### 2) Carga y unión de datos de detenidos (2021–2024) ####
detenidos <- imap_dfr(
  c(
    "2021" = "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Detenidos2021.sav",
    "2022" = "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Detenidos2022.xlsx",
    "2023" = "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Detenidos2023.xlsx",
    "2024" = "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Detenidos1T2024.xlsx"),
  ~ leer_github(.x, paste0(".", tools::file_ext(.x))) %>%
    mutate(across(where(is.numeric), as.character),
           Año = as.integer(.y)))

#### 3) Diccionario y recodificación de variables clave ####
dic <- leer_github(
  "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Diccionario%20Datos.xlsx",
  ".xlsx")

colnames(dic) <- c("variable", "codigo", "etiqueta")

detenidos <- detenidos %>%
  mutate(
    mes_ocu = dplyr::recode(mes_ocu,
                               !!!set_names(
                                 dic$etiqueta[dic$variable=="MES DE OCURRENCIA"],
                                 dic$codigo  [dic$variable=="MES DE OCURRENCIA"])),
    depto_ocu = dplyr::recode(depto_ocu,
                               !!!set_names(
                                 dic$etiqueta[dic$variable=="DEPARTAMENTO DE OCURRENCIA"],
                                 dic$codigo  [dic$variable=="DEPARTAMENTO DE OCURRENCIA"])),
    delito_com = dplyr::recode(delito_com,
                               !!!set_names(
                                 dic$etiqueta[dic$variable=="CAUSA COMETIDA"],
                                 dic$codigo  [dic$variable=="CAUSA COMETIDA"])))

#### 4) Población nacional (Censo 2018) ####
censo_raw <- leer_github(
  "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Bukele/Datos/Censo2018.xlsx",
  ".xlsx", sheet = "A1_2", skip = 4)

# Extraer directamente la columna de población total censada
pob_nacional <- sum(censo_raw[["Población total censada"]], na.rm = TRUE)

#### 5) Incidencia nacional POR TIPO DE DELITO ####
incidencia_tipo <- detenidos %>%
  count(Año, mes_ocu, delito_com, name = "casos") %>%
  mutate(incidencia = casos / pob_nacional * 100000)

#### 6) Incidencia nacional TOTAL DE DELITOS ####
incidencia_total <- incidencia_tipo %>%
  group_by(Año, mes_ocu) %>%
  summarise(casos = sum(casos),
            incidencia = casos / pob_nacional * 100000,
            .groups = "drop")

#### 7) Crear columna fecha para series de tiempo ####
meses <- c(Enero=1, Febrero=2, Marzo=3, Abril=4,
           Mayo=5, Junio=6, Julio=7, Agosto=8,
           Septiembre=9, Octubre=10, Noviembre=11, Diciembre=12)

incidencia_tipo <- incidencia_tipo %>%
  mutate(fecha = as.Date(paste(Año, meses[mes_ocu], "01", sep = "-")))

incidencia_total <- incidencia_total %>%
  mutate(fecha = as.Date(paste(Año, meses[mes_ocu], "01", sep = "-")))

#### 8) Gráfico de la incidencia total de delitos ####

ggplot(incidencia_total, aes(x = fecha, y = incidencia)) +
  geom_line(size = 1, color = "#BD632F") +
  geom_vline(xintercept = as.Date("2022-03-01"), linetype = "dashed", size = 1) +
  labs(
    title = "Incidencia Total de Delitos en Guatemala",
    x = "Fecha",
    y = "Delitos por cada 100,000 habitantes") +
  scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", hjust = 0.5))


#### 9) Generar y guardar un gráfico de incidencia para cada tipo de delito ####
output_dir <- "C:/Users/marco/OneDrive/Documents/GitHub/ServicioBecario/Bukele/Imágenes/Delitos"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Sólo plotear delitos con al menos 2 observaciones
delitos_validos <- incidencia_tipo %>% 
  count(delito_com) %>% 
  filter(n > 1) %>% 
  pull(delito_com)

for (crime in delitos_validos) {
  df_crime <- incidencia_tipo %>% filter(delito_com == crime)
  min_date  <- min(df_crime$fecha)
  max_date  <- max(df_crime$fecha)
  avg_before <- mean(df_crime$incidencia[df_crime$fecha < as.Date("2022-03-01")], na.rm = TRUE)
  avg_after <- mean(df_crime$incidencia[df_crime$fecha >= as.Date("2022-03-01")], na.rm = TRUE)
  
  p <- ggplot(df_crime, aes(x = fecha, y = incidencia, group = 1)) +
    # Línea de incidencia
    geom_line(color = "#A4243B", size = 1) +
    # Línea de tratamiento
    geom_vline(xintercept = as.Date("2022-03-01"), linetype = "dashed", size = 0.8) +
    # Promedio antes del tratamiento
    annotate("segment",
             x= min_date,
             xend = as.Date("2022-03-01"),
             y = avg_before,
             yend = avg_before,
             linetype = "dotted",
             size = 0.8) +
    # Promedio después del tratamiento
    annotate("segment",
             x = as.Date("2022-03-01"),
             xend = max_date,
             y = avg_after,
             yend = avg_after,
             linetype = "dotted",
             size = 0.8) +
    labs(
      title = paste0("Incidencia de '", crime, "' en Guatemala"),
      subtitle = "Tratamiento: marzo de 2022",
      x = "Fecha",
      y = "Delitos por cada 100,000 habitantes") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5))
  
  # Guardar (sobreescribe si existe)
  ggsave(
    filename = file.path(output_dir, paste0(gsub("[^A-Za-z0-9]+", "_", crime), ".png")),
    plot = p, width    = 10, height   = 6, dpi      = 300)
}

