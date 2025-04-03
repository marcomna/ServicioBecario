###################### Homologación municipios ###############################

# Librerías necesarias
install.packages("pacman")
library(pacman)
library(dplyr)
p_load(readr, tidyverse, stringdist, fuzzyjoin)

# Cargar datos desde la nube para que no tengan que descarlos manualmente en su lap

# URL del archivo CSV
data_url <- "https://raw.githubusercontent.com/marcomna/ServicioBecario/refs/heads/main/Canjes%20de%20Armas/ANEXO%20FOLIO%20330026424002055.csv"

# Cargar los datos en un dataframe
canjes <- read_csv(data_url)

# ¿Cómo están codificados los municipios?
unique(canjes$MUNICIPIO)

# Cargamos el catálogo de municipios y CVEGEOs

# URL del archivo CSV
data_url1 <- "https://github.com/marcomna/ServicioBecario/raw/refs/heads/main/Canjes%20de%20Armas/Cat%C3%A1logo%20CVEGEO/AGEEML_20249161546763.csv"

# Cargar los datos en un dataframe
catálogo <- read_csv(data_url1, locale = locale(encoding = "ISO-8859-1"))

###############################################################################

# Ahora, hay que corregir los nombres municipales de "canjes" para que coincidan con el catálogo

# Primero, homologamos la columna ESTADO de canjes. Es más fácil empezar con esta

# Crear diccionario de equivalencias entre estados en `canjes` y nombres oficiales del INEGI
diccionario_estados <- c(
  "D.F." = "Ciudad de México", "CD. MÉX." = "Ciudad de México", "Cd. de México" = "Ciudad de México",
  "VER." = "Veracruz de Ignacio de la Llave", "Veracruz" = "Veracruz de Ignacio de la Llave",
  "TAMPS." = "Tamaulipas", "Tamaulipas" = "Tamaulipas",
  "QRO." = "Querétaro", "Querétaro" = "Querétaro",
  "CHIH." = "Chihuahua", "Chihuahua" = "Chihuahua",
  "B.C." = "Baja California", "Baja California" = "Baja California",
  "CAMP." = "Campeche", "Campeche" = "Campeche",
  "JAL." = "Jalisco", "Jalisco" = "Jalisco",
  "MEX." = "México", "MÉX." = "México", "México" = "México",
  "DGO." = "Durango", "Durango" = "Durango",
  "GRO." = "Guerrero", "Guerrero" = "Guerrero",
  "HGO." = "Hidalgo", "Hidalgo" = "Hidalgo",
  "CHIS." = "Chiapas", "Chiapas" = "Chiapas",
  "COAH." = "Coahuila de Zaragoza", "Coahuila" = "Coahuila de Zaragoza",
  "N.L." = "Nuevo León", "Nvo. León" = "Nuevo León",
  "Q. ROO" = "Quintana Roo", "Q. ROO." = "Quintana Roo",
  "MICH." = "Michoacán de Ocampo", "Michoacán" = "Michoacán de Ocampo",
  "AGS." = "Aguascalientes", "Aguascalientes" = "Aguascalientes",
  "SON." = "Sonora", "Sonora" = "Sonora",
  "S.L.P." = "San Luis Potosí", "San Luis Potosí" = "San Luis Potosí",
  "YUC." = "Yucatán", "Yucatán" = "Yucatán",
  "SIN." = "Sinaloa", "Sinaloa" = "Sinaloa",
  "COL." = "Colima", "Colima" = "Colima",
  "OAX." = "Oaxaca", "Oaxaca" = "Oaxaca",
  "NAY." = "Nayarit", "Nayarit" = "Nayarit",
  "TAB." = "Tabasco", "Tabasco" = "Tabasco",
  "B.C.S." = "Baja California Sur", "Baja California Sur" = "Baja California Sur",
  "GTO." = "Guanajuato", "Guanajuato" = "Guanajuato",
  "PUE." = "Puebla", "Puebla" = "Puebla",
  "MOR." = "Morelos", "Morelos" = "Morelos",
  "TLAX." = "Tlaxcala", "Tlaxcala" = "Tlaxcala",
  "ZAC." = "Zacatecas", "Zacatecas" = "Zacatecas"
)

# Normalizar nombres de estado en `canjes`
canjes <- canjes %>%
  mutate(ESTADO = str_trim(ESTADO),                # Eliminar espacios extra
         ESTADO = diccionario_estados[ESTADO])     # Reemplazar con nombres oficiales

# Unir con catálogo para obtener CVE_ENT
canjes <- canjes %>%
  left_join(catálogo %>% select(CVE_ENT, NOM_ENT) %>% distinct(), 
            by = c("ESTADO" = "NOM_ENT"))

# Mostrar resumen de coincidencias
table(is.na(canjes$CVE_ENT))  # TRUE indica estados sin coincidencia

# Reorganizar columnas
canjes <- canjes %>% 
  select(`FECHA EVENTO`, ESTADO, CVE_ENT, MUNICIPIO, CORTA, LARGA, TOTAL, CARGS, CARTS, GDAS)


##### Aquí empiecen a hacer la homologación de municipios:

canjes_clean <- canjes %>%
        mutate(
                MUNICIPIO = str_to_title(str_remove(MUNICIPIO, ",\\s*[A-Z.]+$")) # Remove state, capitalize
        ) 


canjes_clean <- canjes_clean %>%
        left_join(
                catálogo %>% select(CVE_MUN, NOM_MUN, NOM_ENT) %>% distinct(),
                by = c("ESTADO" = "NOM_ENT", "MUNICIPIO" = "NOM_MUN")
        )

canjes_clean <- canjes_clean %>%
        select(`FECHA EVENTO`, ESTADO, CVE_ENT, MUNICIPIO, CVE_MUN, everything())

