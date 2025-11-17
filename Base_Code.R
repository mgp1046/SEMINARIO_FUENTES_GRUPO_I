library(eurostat)
library(xml2)
library(rjson)
library(dplyr)
library(tidyjson)

#IMPORTACIÓN DE DATOS DE NEFROLOGÍA

datos_nef_valores <- get_eurostat(search_eurostat("renal")[2], lang = "en")

#Eliminamos la columna freq, que solo tiene un nivel que se repite constantemente
datos_nef_valores$freq <- NULL

#Cambiamos el nombre a la columna icd9cm
datos_nef_valores <- datos_nef_valores %>%
  rename(enfermedades = icd9cm)

#Cambiamos el nombre a la columna TIME_PERIOD
datos_nef_valores <- datos_nef_valores %>% 
  rename(date = TIME_PERIOD)


datos_nef_codigos <- get_eurostat(search_eurostat("renal")[2], lang = "en", type = "label")
datos_nef_codigos$freq <- NULL

datos_nef_codigos <- datos_nef_codigos %>%
  rename(enfermedades = icd9cm)

datos_nef_codigos <- datos_nef_codigos %>% 
  rename(date = TIME_PERIOD)

#Visualización de estructura
str(datos_nef_valores)
str(datos_nef_codigos)





#IMPORTACIÓN DE DATOS DE CALIDAD DEL AGUA
agua_data <- fromJSON(file = "Data/DataExtract.json")

agua_data_framed <- spread_all(agua_data)
# Hay 288.711 registros para 26 atributos, claramente demasiados, filtramos

str(agua_data_framed)

agua_data_filtered <- agua_data_framed %>% select(., cYear, fileUrl, euRBDCode, rbdName, euSubUnitCode, surfaceWaterBodyName, cArea, 
                            surfaceWaterBodyCategory, reservoir, hasDescriptiveData, swEcologicalStatusOrPotentialValue, 
                            swChemicalStatusValue) %>% dplyr::rename(., "Area_(km2)" = cArea)
#Visualización de estructura
str(agua_data_filtered)
