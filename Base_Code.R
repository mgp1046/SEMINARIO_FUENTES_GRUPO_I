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

#Cambiamos formato de fecha
datos_nef_valores$date <- format(as.Date(datos_nef_valores$date), "%Y")

#Convertimos date a numeric
datos_nef_valores$date <- as.numeric(datos_nef_valores$date)


datos_nef_codigos <- get_eurostat(search_eurostat("renal")[2], lang = "en", type = "label")
datos_nef_codigos$freq <- NULL

datos_nef_codigos <- datos_nef_codigos %>%
  rename(enfermedades = icd9cm)

datos_nef_codigos <- datos_nef_codigos %>% 
  rename(date = TIME_PERIOD)

datos_nef_codigos$date <- format(as.Date(datos_nef_codigos$date), "%Y")

datos_nef_codigos$date <- as.numeric(datos_nef_codigos$date)


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


#Vemos que tipos de cuerpos de agua hay
#unique(agua_data_filtered$surfaceWaterBodyCategory)

#Quitamos las masas de agua costera "CW" y de agua maritima territorial "TeW"
agua_data_filtered <- agua_data_filtered %>%
  filter(!surfaceWaterBodyCategory %in% c("CW", "TeW"))

unique(agua_data_filtered$surfaceWaterBodyCategory)

#Visualización de estructura
str(agua_data_filtered)