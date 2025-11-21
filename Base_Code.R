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
# Hay 288.711 registros para 26 atributos, claramente demasiados, filtramos
agua_data <- fromJSON(file = "Data/DataExtract.json") %>% 
  spread_all(.) %>%
  select(., cYear, fileUrl, euRBDCode, rbdName, euSubUnitCode, surfaceWaterBodyName, cArea, surfaceWaterBodyCategory,
         reservoir, hasDescriptiveData, swEcologicalStatusOrPotentialValue, swChemicalStatusValue) %>%
  dplyr::rename(., "Area_(km2)" = cArea)

str(agua_data)

#Vemos que tipos de cuerpos de agua hay
#unique(agua_data_filtered$surfaceWaterBodyCategory)

#Quitamos las masas de agua costera "CW" y de agua maritima territorial "TeW"
agua_data <- agua_data %>%
  filter(!surfaceWaterBodyCategory %in% c("CW", "TeW"))

unique(agua_data$surfaceWaterBodyCategory)

#Visualización de estructura
str(agua_data)
colnames(agua_data)

?lapply
# Comprobamos que no haya valores nulos o vacios en la columna del XML
anyNA(agua_data$fileUrl)

# Eliminamos los registros con valores nulos para el xml

agua_data <- agua_data %>%
  filter(!is.na(.data[["fileUrl"]]) & .data[["fileUrl"]] != "")

# Creamos una función para leer los xml a partir de su url

xml_search <- function (url){
  xml <- read_xml(url)
  interes <- list(
    campo1 = xml_text(xml_find_first(xml, "nombre_campo")),
    campo2 =xml_text(xml_find_first(xml, "nombre_campo"))
  )
  
  rm(xml)
  gc()
  
  return(interes)
}
agua_data$xml_data <- lapply(agua_data[["fileUrl"]], read_xml)

colnames(agua_data)

#COMPARACION DE PAISES ENTRE AMBAS BASES DE DATOS

#Vemos que paises tenemos en cada base de datos
agua_data$countryCode <- sapply(agua_data$..JSON, function(x) x$countryCode)
unique(agua_data$countryCode)
unique(datos_nef_valores$geo)

paises_agua   <- unique(agua_data$countryCode)
paises_nefro  <- unique(datos_nef_valores$geo)

#Hacemos la interseccion para ver cuales son comunes
paises_comunes <- intersect(paises_agua, paises_nefro)
paises_comunes

