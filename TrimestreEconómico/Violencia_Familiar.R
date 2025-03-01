###TRIMESTRE ECONÓMICO###

library(readxl)
library(tidyverse)
library(dplyr)
library(readr)
library(viridis)
library(zoo)
library(imputeTS)
library(lubridate)

# Base de datos Delitos
guess_encoding("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/IDM_NM_dic24.csv")
delitos <- read.csv("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/IDM_NM_dic24.csv", sep=",",
                    fileEncoding = "ISO-8859-1")


#Filtrar por violencia familiar
delitos_violencia_familiar <- delitos %>%
  filter(Tipo.de.delito == "Violencia familiar")


#Cambiar nombres de columnas
colnames(delitos_violencia_familiar)

delitos_violencia_familiar <- delitos_violencia_familiar %>%
  rename(Clv_Municipio = Cve..Municipio,
         Clv_Entidad = Clave_Ent, 
         `Bien Jurídico Afectado` = Bien.jurídico.afectado, 
         Tipo_Delito = Tipo.de.delito,
         Subtipo_Delito = Subtipo.de.delito)

colnames(delitos_violencia_familiar)

# Arreglo base delitos y fechas
delitos_violencia_familiar <- delitos_violencia_familiar %>% 
  gather(mes, valor, -c(Año, Clv_Entidad, Entidad, Clv_Municipio, Municipio, `Bien Jurídico Afectado`, Tipo_Delito, Subtipo_Delito, Modalidad))

delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Enero"] <- 1
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Febrero"] <- 2
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Marzo"] <- 3
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Abril"] <- 4
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Mayo"] <- 5
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Junio"] <- 6
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Julio"] <- 7
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Agosto"] <- 8
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Septiembre"] <- 9
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Octubre"] <- 10
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Noviembre"] <- 11
delitos_violencia_familiar$mes[delitos_violencia_familiar$mes == "Diciembre"] <- 12

delitos_violencia_familiar$Fecha <- as.yearmon(paste(delitos_violencia_familiar$Año, delitos_violencia_familiar$mes), "%Y %m") #CAMBIAR AÑO DE ORDEN %Y- %M
delitos_violencia_familiar$Fecha <- format(delitos_violencia_familiar$Fecha,
                        "%m-%Y")
delitos_violencia_familiar$Fecha <- my(delitos_violencia_familiar$Fecha)


#Importar base de datos de población 2020
poblacion <- read.csv("C:/Users/pipeg/OneDrive/Escritorio/Investigación Marco/Bukele/ITER_NAL_2020_csv (1)/ITER_NALCSV20.csv")
colnames(poblacion)

# Lista de columnas a eliminar
columnas_a_eliminar <- c("LONGITUD", "LATITUD", "ALTITUD","POBFEM", "POBMAS", "P_0A2", "P_0A2_F", "P_0A2_M",
                         "P_3YMAS", "P_3YMAS_F", "P_3YMAS_M", "P_5YMAS", "P_5YMAS_F", "P_5YMAS_M",
                         "P_12YMAS", "P_12YMAS_F", "P_12YMAS_M", "P_15YMAS", "P_15YMAS_F", "P_15YMAS_M",
                         "P_18YMAS", "P_18YMAS_F", "P_18YMAS_M", "P_3A5", "P_3A5_F", "P_3A5_M", "P_6A11",
                         "P_6A11_F", "P_6A11_M", "P_8A14", "P_8A14_F", "P_8A14_M", "P_12A14",
                         "P_12A14_F", "P_12A14_M", "P_15A17", "P_15A17_F", "P_15A17_M", "P_18A24",
                         "P_18A24_F", "P_18A24_M", "P_15A49_F", "P_60YMAS", "P_60YMAS_F", "P_60YMAS_M",
                         "REL_H_M", "POB0_14", "POB15_64", "POB65_MAS", "P_0A4", "P_0A4_F", "P_0A4_M",
                         "P_5A9", "P_5A9_F", "P_5A9_M", "P_10A14", "P_10A14_F", "P_10A14_M",
                         "P_15A19", "P_15A19_F", "P_15A19_M", "P_20A24", "P_20A24_F", "P_20A24_M",
                         "P_25A29", "P_25A29_F", "P_25A29_M", "P_30A34", "P_30A34_F", "P_30A34_M",
                         "P_35A39", "P_35A39_F", "P_35A39_M", "P_40A44", "P_40A44_F", "P_40A44_M",
                         "P_45A49", "P_45A49_F", "P_45A49_M", "P_50A54", "P_50A54_F", "P_50A54_M",
                         "P_55A59", "P_55A59_F", "P_55A59_M", "P_60A64", "P_60A64_F", "P_60A64_M",
                         "P_65A69", "P_65A69_F", "P_65A69_M", "P_70A74", "P_70A74_F", "P_70A74_M",
                         "P_75A79", "P_75A79_F", "P_75A79_M", "P_80A84", "P_80A84_F", "P_80A84_M",
                         "P_85YMAS", "P_85YMAS_F", "P_85YMAS_M", "PROM_HNV", "PNACENT", "PNACENT_F",
                         "PNACENT_M", "PNACOE", "PNACOE_F", "PNACOE_M", "PRES2015", "PRES2015_F",
                         "PRES2015_M", "PRESOE15", "PRESOE15_F", "PRESOE15_M", "P3YM_HLI",
                         "P3YM_HLI_F", "P3YM_HLI_M", "P3HLINHE", "P3HLINHE_F", "P3HLINHE_M",
                         "P3HLI_HE", "P3HLI_HE_F", "P3HLI_HE_M", "P5_HLI", "P5_HLI_NHE", "P5_HLI_HE",
                         "PHOG_IND", "POB_AFRO", "POB_AFRO_F", "POB_AFRO_M", "PCON_DISC", "PCDISC_MOT",
                         "PCDISC_VIS", "PCDISC_LENG", "PCDISC_AUD", "PCDISC_MOT2", "PCDISC_MEN",
                         "PCON_LIMI", "PCLIM_CSB", "PCLIM_VIS", "PCLIM_HACO", "PCLIM_OAUD", "PCLIM_MOT2",
                         "PCLIM_RE_CO", "PCLIM_PMEN", "PSIND_LIM", "P3A5_NOA", "P3A5_NOA_F",
                         "P3A5_NOA_M", "P6A11_NOA", "P6A11_NOAF", "P6A11_NOAM", "P12A14NOA",
                         "P12A14NOAF", "P12A14NOAM", "P15A17A", "P15A17A_F", "P15A17A_M", "P18A24A",
                         "P18A24A_F", "P18A24A_M", "P8A14AN", "P8A14AN_F", "P8A14AN_M", "P15YM_AN",
                         "P15YM_AN_F", "P15YM_AN_M", "P15YM_SE", "P15YM_SE_F", "P15YM_SE_M",
                         "P15PRI_IN", "P15PRI_INF", "P15PRI_INM", "P15PRI_CO", "P15PRI_COF",
                         "P15PRI_COM", "P15SEC_IN", "P15SEC_INF", "P15SEC_INM", "P15SEC_CO",
                         "P15SEC_COF", "P15SEC_COM", "P18YM_PB", "P18YM_PB_F", "P18YM_PB_M",
                         "GRAPROES", "GRAPROES_F", "GRAPROES_M", "PEA", "PEA_F", "PEA_M",
                         "PE_INAC", "PE_INAC_F", "PE_INAC_M", "POCUPADA", "POCUPADA_F", "POCUPADA_M",
                         "PDESOCUP", "PDESOCUP_F", "PDESOCUP_M", "PSINDER", "PDER_SS", "PDER_IMSS",
                         "PDER_ISTE", "PDER_ISTEE", "PAFIL_PDOM", "PDER_SEGP", "PDER_IMSSB",
                         "PAFIL_IPRIV", "PAFIL_OTRAI", "P12YM_SOLT", "P12YM_CASA", "P12YM_SEPA",
                         "PCATOLICA", "PRO_CRIEVA", "POTRAS_REL", "PSIN_RELIG", "TOTHOG", "HOGJEF_F",
                         "HOGJEF_M", "POBHOG", "PHOGJEF_F", "PHOGJEF_M", "VIVTOT", "TVIVHAB",
                         "TVIVPAR", "VIVPAR_HAB", "VIVPARH_CV", "TVIVPARHAB", "VIVPAR_DES",
                         "VIVPAR_UT", "OCUPVIVPAR", "PROM_OCUP", "PRO_OCUP_C", "VPH_PISODT",
                         "VPH_PISOTI", "VPH_1DOR", "VPH_2YMASD", "VPH_1CUART", "VPH_2CUART",
                         "VPH_3YMASC", "VPH_C_ELEC", "VPH_S_ELEC", "VPH_AGUADV", "VPH_AEASP",
                         "VPH_AGUAFV", "VPH_TINACO", "VPH_CISTER", "VPH_EXCSA", "VPH_LETR",
                         "VPH_DRENAJ", "VPH_NODREN", "VPH_C_SERV", "VPH_NDEAED", "VPH_DSADMA",
                         "VPH_NDACMM", "VPH_SNBIEN", "VPH_REFRI", "VPH_LAVAD", "VPH_HMICRO",
                         "VPH_AUTOM", "VPH_MOTO", "VPH_BICI", "VPH_RADIO", "VPH_TV", "VPH_PC",
                         "VPH_TELEF", "VPH_CEL", "VPH_INTER", "VPH_STVP", "VPH_SPMVPI", "VPH_CVJ",
                         "VPH_SINRTV", "VPH_SINLTC", "VPH_SINCINT", "VPH_SINTIC", "TAMLOC")

# Eliminar las columnas especificadas
poblacion <- poblacion %>%
  select(-all_of(columnas_a_eliminar))

# Filtrar primero por localidades igual a 0 y luego por municipios diferentes de 0, para usar el total por municipio
poblacion <- poblacion %>%
  filter(LOC == 0, MUN != 0)

#Nueva columna con clave para hacer el joint
poblacion <- poblacion_filtrada %>%
  mutate(ID_Municipio = (ENTIDAD * 1000) + MUN)

# Realizar el join de la columna POBTOT de poblacion a la tabla delitos
delitos_violencia_familiar <- delitos_violencia_familiar %>%
  left_join(poblacion %>% select(Clv_Municipio = ID_Municipio, POBTOT), 
            by = "Clv_Municipio")

#Sacar incidencia delictiva
delitos_violencia_familiar <- delitos_violencia_familiar %>%
  mutate(incidencia = (valor / POBTOT) * 100000)


#Agrupar por trimestre
delitos_violencia_familiar <- delitos_violencia_familiar %>%
  mutate(trimestre = paste0(year(Fecha), "Q", quarter(Fecha))) %>%  # Crear la columna de trimestre
  group_by(trimestre, Clv_Municipio, Municipio, `Bien Jurídico Afectado`, Tipo_Delito, Subtipo_Delito, Modalidad, Clv_Entidad, Entidad) %>%
  summarise(
    valor = sum(valor, na.rm = TRUE),  # Sumar los delitos en cada trimestre por municipio
    POBTOT = first(POBTOT),  # Mantener la población del municipio
    incidencia = (valor / POBTOT) * 100000,  # Recalcular la incidencia
    .groups = "drop"
  )

#Reorganizar columnas
delitos_violencia_familiar <- delitos_violencia_familiar %>%
  select(Tipo_Delito, Subtipo_Delito, Modalidad, `Bien Jurídico Afectado`, 
         Clv_Entidad, Entidad, Clv_Municipio, Municipio, trimestre,
         valor, POBTOT, incidencia)




