###################### Homologación municipios ###############################

# Librerías necesarias
library(pacman)
p_load(readr, tidyverse, stringdist, fuzzyjoin, stringr, stringi, fuzzyjoin, sf, wesanderson)

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

# Quitar estado de columna municipio y capitalizar
canjes_clean <- canjes %>%
  mutate(
    MUNICIPIO = str_to_title(str_remove(MUNICIPIO, ",.*$")),
    MUNICIPIO = str_to_lower(MUNICIPIO),
    MUNICIPIO = stri_trans_general(MUNICIPIO, "Latin-ASCII"),
    MUNICIPIO = str_squish(MUNICIPIO),
    ESTADO = stri_trans_general(ESTADO, "Latin-ASCII"),
    ESTADO = str_to_lower(ESTADO),
    ESTADO = str_squish(ESTADO)
  )

## Quitar acentos
catálogo_clean <- catálogo %>%
  mutate(
    NOM_MUN = stri_trans_general(NOM_MUN, "Latin-ASCII"),
    NOM_MUN = str_to_lower(NOM_MUN),
    NOM_MUN = str_squish(NOM_MUN),
    NOM_ENT = stri_trans_general(NOM_ENT, "Latin-ASCII"),
    NOM_ENT = str_to_lower(NOM_ENT),
    NOM_ENT = str_squish(NOM_ENT)
  )


canjes_clean <- canjes_clean %>%
  left_join(
    catálogo_clean %>% select(CVE_MUN, NOM_MUN, NOM_ENT) %>% distinct(),
    by = c("ESTADO" = "NOM_ENT", "MUNICIPIO" = "NOM_MUN"),
    relationship = "many-to-many"
  )

canjes_clean <- canjes_clean %>%
  select(`FECHA EVENTO`, ESTADO, CVE_ENT, MUNICIPIO, CVE_MUN, everything())


# Named vector of abbreviations → full names (all lowercase, no accents)
abbrev_to_full <- c(
  "ags."       = "aguascalientes",
  "bc."        = "baja california",
  "bcs."       = "baja california sur",
  "camp."      = "campeche",
  "chis."      = "chiapas",
  "chih."      = "chihuahua",
  "coah."      = "coahuila",
  "col."       = "colima",
  "dgo."       = "durango",
  "gto."       = "guanajuato",
  "gro."       = "guerrero",
  "hgo."       = "hidalgo",
  "jal."       = "jalisco",
  "mex."       = "mexico",
  "mich."      = "michoacan",
  "mor."       = "morelos",
  "nay."       = "nayarit",
  "nl."        = "nuevo leon",
  "oax."       = "oaxaca",
  "pue."       = "puebla",
  "qro."       = "queretaro",
  "q. roo."    = "quintana roo",
  "slp."       = "san luis potosi",
  "sin."       = "sinaloa",
  "son."       = "sonora",
  "tab."       = "tabasco",
  "tamps."     = "tamaulipas",
  "tlax."      = "tlaxcala",
  "ver."       = "veracruz",
  "yuc."       = "yucatan",
  "zac."       = "zacatecas",
  "dr."        = "doctor", 
  "gral."      = "general"
)

# Apply replacements based on match
canjes_clean <- canjes_clean %>%
  mutate(
    MUNICIPIO = if_else(
      MUNICIPIO %in% names(abbrev_to_full),
      abbrev_to_full[MUNICIPIO],
      MUNICIPIO
    )
  )

canjes_clean <- canjes_clean %>%
  mutate(MUNICIPIO = str_replace_all(MUNICIPIO, 
                                     pattern = paste(names(abbrev_to_full), collapse = "|"), 
                                     replacement = function(x) ifelse(x %in% names(abbrev_to_full), abbrev_to_full[x], x)))


# A tibble or dataframe of manual corrections
manual_fixes <- tribble(
  ~ESTADO,~MUNICIPIO,~CVE_MUN,
  "durango","durango","005",
  "aguascalientes", "aguascalientes","001",
  "puebla","puebla","114",
  "zacatecas","zacatecas","056",
  "chihuahua","hidalgo del parral","032",
  "oaxaca","oaxaca de juarez","067",
  "ciudad de mexico","miguel hidalgo","016",
  "chihuahua","chihuahua","019",
  "colima","colima","002",
  "jalisco","tlaquepaque","098",
  "tlaxcala","tlaxcala","033",
  "nuevo leon","general escobedo","021",
  "ciudad de mexico","cuajimalpa de morelos","004",
  "mexico","ecatepec de morelos","033",
  "veracruz de ignacio de la llave","veracruz","193",
  "sinaloa","sinaloa","017",
  "veracruz de ignacio de la llave","poza rica de hidalgo","131",
  "campeche","campeche","003",
  "veracruz de ignacio de la llave","tuxpam","189",
  "chiapas","cintalapa","017",
  "veracruz de ignacio de la llave","alamo","160",
  "nuevo leon","carmen","010",
  "guanajuato","guanajuato","015",
  "nuevo leon","montemorelos","038",
  "nuevo leon","doctor arroyo","014",
  "nuevo leon","general bravo","020",
  "nuevo leon","sabinas hidalgo","044",
  "nuevo leon","hidalgo","047",
  "oaxaca","juchitan de zaragoza","043",
  "mexico","san simon de guerrero","077",
  #nombre correcto es atltzayanca
  "tlaxcala","altzayanca","004",
  "michoacan de ocampo","hidalgo","034",
  #nombre correcto es yauhquemehcan
  "tlaxcala","yauhquemecan","043",
  "veracruz de ignacio de la llave","medellin","105",
  "coahuila de zaragoza","cuatrocienegas","007",
  #municipio es san juan de sabinas, no nueva rosita
  "coahuila de zaragoza","nueva rosita","032",
  "mexico","acambay","001",
  "nuevo leon","general zuazua","025",
  "oaxaca","ocotlan de morelos","068",
  #nombre correcto es ziltlaltepec
  "tlaxcala","zitlaltepec de trinidad sanchez santos","037",
  "mexico","villa guerrero","113",
  "nuevo leon","general teran","022",
  "tlaxcala","santa cruz tlaxcala","026",
  "mexico","morelos","056",
  "nuevo leon","doctor coss","015",
  "nuevo leon","doctor gonzalez","016",
  "nuevo leon","general zaragoza","024",
  "san luis potosi","ahualulco","001",
  "zacatecas","tabasco","044",
  "coahuila de zaragoza","morelos","019",
  "jalisco","tepatitlan de morelos","093",
  "chihuahua","praxedis g. guerrero","053",
  "guerrero","alcozauca de guerrero","004",
  "guerrero","tixtla de guerrero","061",
  "jalisco","san martin hidalgo","077",
  "nuevo leon","general trevino", "023",
  "oaxaca","tezoatlan de segura y luna","549",
  "puebla","canada morelos","099",
  "sinaloa","navolato","018",
  "tamaulipas","guerrero","014",
  #nombre correcto es ozuluama de mascarenas
  "veracruz de ignacio de la llave","ozuluama de mascare+/-as","121",
  "yucatan","quintana roo","060"
)

canjes_clean <- canjes_clean %>%
  left_join(manual_fixes, by = c("ESTADO", "MUNICIPIO"), suffix = c("", "_fix")) %>%
  mutate(
    CVE_MUN = coalesce(CVE_MUN_fix, CVE_MUN)  # Use fixed value when available
  ) %>%
  select(-CVE_MUN_fix)  # Clean up the extra column


# Con el siguiente código, podemos ver si hay observaciones en donde no se haya encontrado la CVE_MUN
no_encontrados <- canjes_clean %>% 
  filter(is.na(CVE_MUN))

# Lo siguiente muestra la frecuencia de observaciones en donde hay NA en CVE_MUN
no_encontrados_frec <- canjes_clean %>%
  filter(is.na(CVE_MUN)) %>%
  count(ESTADO,MUNICIPIO, name = "frecuencia") %>%
  arrange(desc(frecuencia))

##############################################################################

### Calcular número de armas canjeadas por mes por cada 100 mil habitantes

canjes_clean <- canjes_clean %>%
  rename(TOTAL_A = TOTAL) %>% 
  mutate(TOTAL = rowSums(across(c(TOTAL_A,CARGS,CARTS,GDAS), ~ .), na.rm = TRUE))

# 1. Definir la URL y el archivo destino
options(timeout = 600)
url_zip   <- "https://www.inegi.org.mx/contenidos/programas/ccpv/2020/microdatos/iter/ITER_NAL_2020_csv.zip"
zip_local <- tempfile(fileext = ".zip")

# 2. Descargar el ZIP (modo binario para Windows)
download.file(url_zip, zip_local, mode = "wb")

# 3. Descomprimir en un directorio temporal
dir_unzip <- tempdir()
unzip(zip_local, exdir = dir_unzip)

# 4. Localizar el(os) CSV dentro del ZIP
csv_path <- list.files(dir_unzip, pattern = "ITER_NAL.*\\.csv$", full.names = TRUE)

# 5. Leer el CSV en 'poblacion' (usa readr para velocidad y parseo automático)
if (!requireNamespace("readr", quietly = TRUE)) install.packages("readr")
poblacion <- read_csv(csv_path)

##### LIMPIEZA #######
# 1) Identifica índices de POBTOT hasta el final
cols <- which(names(poblacion)=="POBTOT"):ncol(poblacion)

# 2) Para cada una, si es character, reemplaza "*" por NA
for(i in cols) {
  if (is.character(poblacion[[i]])) {
    poblacion[[i]][ poblacion[[i]] == "*" ] <- NA
  }
}

# Quitamos NAs en columna LONGITUD (porque indica que es una observación que es una suma parcial de estado, país, etc.)
poblacion <- poblacion %>%
  filter(!is.na(LONGITUD))

#### COLAPSO DE POBLACIÓN SUMANDO POR MUN #####

poblacion_agg <- poblacion %>%
  group_by(ENTIDAD, MUN) %>% 
  summarise(
    POBTOT = sum(as.numeric(POBTOT), na.rm = TRUE),
    .groups = "drop")

#### Código para corroborar población por entidad federativa ####
poblacion_agg %>%
  filter(ENTIDAD == "09") %>% 
  summarise(POBTOT = sum(POBTOT, na.rm = TRUE))

# Agregar población a df de canjes

poblacion_agg <- poblacion_agg %>%
  rename(CVE_ENT = ENTIDAD) %>% 
  rename(CVE_MUN = MUN)

canjes_clean <- canjes_clean %>%
  left_join(poblacion_agg, by = c("CVE_ENT", "CVE_MUN"))

canjes_clean <- canjes_clean %>%
  mutate(
    CVE_ENT = str_pad(CVE_ENT, 2, pad = "0"),
    CVE_MUN = str_pad(CVE_MUN, 3, pad = "0")
  )

poblacion_agg <- poblacion_agg %>%
  mutate(
    CVE_ENT = str_pad(CVE_ENT, 2, pad = "0"),
    CVE_MUN = str_pad(CVE_MUN, 3, pad = "0")
  )


# Calcular armas canjeadas por cada 100 mil habitantes
canjes_clean <- canjes_clean %>%
  mutate(RATIO = (TOTAL / POBTOT) * 100000)


##############################################################################

#### Mapas

# Create a continuous version of the Zissou1 palette
pal <- colorRampPalette(wes_palette("Zissou1"))(100)

mun_sf <- st_read("~/Downloads/Escuela/Carrera/Servicio Becario Marco/Conabio Data/mun22cw.shp")

# Check the projection
st_crs(mun_sf)


# Ensure join keys are the same type
mun_sf <- mun_sf %>%
  mutate(CVE_ENT = as.character(CVE_ENT),
         CVE_MUN = as.character(CVE_MUN))

# Join your data
map_data <- mun_sf %>%
  left_join(canjes_clean, by = c("CVE_ENT", "CVE_MUN"))

# Plot the firearms per 100k
ggplot(map_data) +
  geom_sf(aes(fill = RATIO), color = NA) +
  scale_fill_gradientn(colors = pal, trans = "log", na.value = "grey90")+
  theme_minimal() +
  labs(
    title = "Armas canjeadas por cada 100 mil habitantes",
    fill = "Canjes / 100k"
  )

