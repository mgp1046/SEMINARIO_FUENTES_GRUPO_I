# Importacion de las librerias necesarias ------------

library(eurostat)
library(xml2)
library(rjson)
library(dplyr)
library(tidyjson)
library(ggplot2)
library(tidyr)
library(jsonlite)
library(furrr)
library(purrr)
library(future.apply)
library(plotly)

# IMPORTACIÓN DE DATOS DE NEFROLOGÍA --------------------

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

#IMPORTACIÓN DE DATOS DE CALIDAD DEL AGUA ----------------------
# Hay 288.711 registros para 26 atributos, claramente demasiados, filtramos
agua_data <- jsonlite::fromJSON("Data/DataExtract.geojson", simplifyVector = FALSE)
  

agua_data <- lapply(agua_data$features, function(feature){
    pro <- feature$properties
    
    pro <- lapply(pro, function(x) if (is.null(x)) NA else x)
    
    as.data.frame(pro, stringAsFactor = FALSE)
  }) %>%
  bind_rows(.) 
agua_data <- select(agua_data, cYear, countryCode, fileUrl, euRBDCode, rbdName, euSubUnitCode, surfaceWaterBodyName, cArea, surfaceWaterBodyCategory,
         reservoir, hasDescriptiveData, swEcologicalStatusOrPotentialValue, swChemicalStatusValue) %>%
  dplyr::rename(., "Area_(km2)" = cArea) %>%
  filter(!surfaceWaterBodyCategory %in% c("CW", "TeW"))

#Vemos que tipos de cuerpos de agua hay
#unique(agua_data_filtered$surfaceWaterBodyCategory)

#Quitamos las masas de agua costera "CW" y de agua maritima territorial "TeW"

unique(agua_data$surfaceWaterBodyCategory)

#Visualización de estructura
str(agua_data)
colnames(agua_data)


#DESDE AQUÍ, HASTA LÍNEA 136 NO ESTÁ EN RMD. DE LA 169 A 174 TAMPOCO
# Comprobamos que no haya valores nulos o vacios en la columna del XML
anyNA(agua_data$fileUrl)

# Eliminamos los registros con valores nulos para el xml
agua_data <- agua_data %>%
  filter(!is.na(.data[["fileUrl"]]) & .data[["fileUrl"]] != "")

# Eliminamos espacio sin usar del entorno
gc()

# Leer el primer XML
xml1 <- read_xml(agua_data$fileUrl[1])

# Mostrar los nodos principales
xml_find_all(xml1, ".//*")


# Creamos una función para leer los xml a partir de su url
xml_search <- function(url) {

  
  tryCatch({
    xml <- read_xml(url)
    data.frame(
      Impactos = paste(xml_text(xml_find_all(xml, ".//swSignificantImpactType")), collapse = "; "),
      Puntuacion_ecologica = as.numeric(xml_text(xml_find_first(xml, ".//swEcologicalStatusOrPotentialValue"))),
      Puntuacion_quimica = as.numeric(xml_text(xml_find_first(xml, ".//swChemicalStatusValue"))),
      stringsAsFactors = FALSE
    )
    }, error = function(){
    message("Error leyendo XML")
    data.frame(Puntuacion_ecologica = NA, Impactos = NA, Puntuacion_quimica = NA)
  })
}

# NO EJECUTAR HASTA TENER TODOS LOS FILTRADOS DE LA SIGUIENTE SECCION

tamano_bloque <- 5000

bloque <- agua_data$fileUrl[1:tamano_bloque]
paises <- agua_data$countryCode[1:tamano_bloque]
years <- agua_data$cYear[1:tamano_bloque]

bloque <- future_lapply(bloque, xml_search)

xml <- do.call(rbind, bloque)
xml$countryCode <- paises
xml$date <- as.numeric(years)

colnames(agua_data)

# Guardamos el objeto de R para que en futuras ejecuciones no tarde tanto
saveRDS(object = agua_data, file = "Data/Datos_calidad_agua.rds")

# Carga de los datos SIN EJECUTAR TODO

agua_data <- readRDS(file = "Data/Datos_calidad_agua.rds")


#COMPARACION DE PAISES ENTRE AMBAS BASES DE DATOS -----------------------------

#Vemos que paises tenemos en cada base de datos

unique(agua_data$countryCode)
unique(datos_nef_valores$geo)

paises_agua   <- unique(agua_data$countryCode)
paises_nefro  <- unique(datos_nef_valores$geo)

#Hacemos la interseccion para ver cuales son comunes
paises_comunes <- intersect(paises_agua, paises_nefro)
paises_comunes


#Filtramos SOLO los países comunes
agua_data <- agua_data %>% filter(countryCode %in% paises_comunes) 
datos_nef_valores <- datos_nef_valores %>% filter(geo %in% paises_comunes)
str(datos_nef_valores)
str(agua_data)


# agua_data <- mutate(agua_data, Puntuacion_ecologica = map_dbl(xml_data, ~ {
#   valor <- .x[["Puntuacion_ecologica"]]
#   if(is.null(valor) || length(valor) == 0) return(NA)
#   as.numeric(valor)
# }),
# date = as.numeric(cYear))


# Unión 
datos_combinados <- left_join(agua_data, datos_nef_valores, 
            by = c("countryCode" = "geo"))

str(datos_combinados)

#Resumen datos de agua por país
agua_resumen <- agua_data %>%
  group_by(countryCode) %>%
  summarise(
    Area_total = sum(`Area_(km2)`, na.rm = TRUE),
    swEcologicalStatus_promedio = mean(
      as.numeric(swEcologicalStatusOrPotentialValue[grepl("^[0-9]+$", swEcologicalStatusOrPotentialValue)]),
      na.rm = TRUE
    ),
    swChemicalStatus_promedio = mean(
      as.numeric(swChemicalStatusValue[grepl("^[0-9]+$", swChemicalStatusValue)]),
      na.rm = TRUE
    )
  )

#Resumen datos renales por país
nefro_resumen <- datos_nef_valores %>%
  group_by(geo) %>%
  summarise(total_casos_renales = sum(values, na.rm = TRUE))

#Unión final de los datos resumidos
datos_combinados_final <- agua_resumen %>%
  full_join(nefro_resumen, by = c("countryCode" = "geo"))
str(datos_combinados_final)
View(datos_combinados_final)

#Casos renales vs estado ecológico del agua
ggplot(datos_combinados_final, aes(x = swEcologicalStatus_promedio, y = total_casos_renales)) +
  geom_point(aes(size = Area_total, color = countryCode)) +
  labs(
    x = "Estado ecológico promedio del agua",
    y = "Total de casos renales",
    size = "Área total (km2)",
    color = "País"
  ) +
  theme_minimal()

#Casos renales vs estado químico del agua
ggplot(datos_combinados_final, aes(x = swChemicalStatus_promedio, y = total_casos_renales)) +
  geom_point(aes(size = Area_total, color = countryCode)) +
  labs(
    x = "Estado químico promedio del agua",
    y = "Total de casos renales",
    size = "Área total (km2)",
    color = "País"
 ) +
  theme_minimal()



#GRÁFICO ESTADO QUÍMICO Y ECOLÓGICO VS CASOS RENALES. SIN MAPA

# Crear el gráfico de burbujas
p <- ggplot(datos_combinados_final, aes(
  x = swChemicalStatus_promedio,       # eje x = estado químico
  y = swEcologicalStatus_promedio,     # eje y = estado ecológico
  size = total_casos_renales,          # tamaño = casos renales
  color = swChemicalStatus_promedio,   # color opcional
  text = paste(
    "<b>", countryCode, "</b><br>",
    "Casos renales: ", total_casos_renales, "<br>",
    "Estado ecológico: ", round(swEcologicalStatus_promedio,2), "<br>",
    "Estado químico: ", round(swChemicalStatus_promedio,2)
  )
)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(5, 25)) + # Ajusta según la magnitud de los datos
  scale_color_viridis_c(option = "plasma", na.value = "lightgrey") +
  theme_minimal() +
  labs(
    x = "Estado químico",
    y = "Estado ecológico",
    size = "Casos renales",
    color = "Estado químico"
  )

# Convertir a interactivo
p_interactivo <- ggplotly(p, tooltip = "text")

p_interactivo


# #MAPA INTERACTIVO-----------------
# install.packages(c("sf", "rnaturalearth", "rnaturalearthdata"), type = "binary")
# install.packages(c("units", "lwgeom"))
# install.packages("plotly")
# 
# library(dplyr)
# library(sf)
# library(ggplot2)
# library(plotly)
# library(rnaturalearth)
# library(rnaturalearthdata)
# # 
# 
# 
# # # Cargar mapa europeo
# world <- ne_countries(scale = "medium", returnclass = "sf")
# eu <- world %>% filter(region_un == "Europe")
# 
# # Asegurar columnas compatibles
# eu <- eu %>% rename(countryCode = iso_a2)
# 
# # Unir mapa y los datos
# eu_join <- eu %>%
#   left_join(datos_combinados_final, by = "countryCode")
# 
# # Obtener centroides para ubicar burbujas
# eu_join$centroid <- st_centroid(eu_join$geometry)
# coords <- st_coordinates(eu_join$centroid)
# 
# eu_join$lon <- coords[,1]
# eu_join$lat <- coords[,2]
# 
# # Tooltip
# eu_join <- eu_join %>%
#   mutate(mytext = paste(
#     "<b>", name, "</b><br>",
#     "Casos renales: ", total_casos_renales, "<br>",
#     "Estado ecológico: ", round(swEcologicalStatus_promedio,2), "<br>",
#     "Estado químico: ", round(swChemicalStatus_promedio,2), "<br>",
#     sep = ""
#   ))
# 
# # Mapa estático
# p <- ggplot() +
#   geom_sf(data = eu_join, fill = "grey80", color = "white", alpha = 0.6) +
#   geom_point(
#     data = eu_join,
#     aes(
#       x = lon,
#       y = lat,
#       size = total_casos_renales,
#       color = swChemicalStatus_promedio,
#       text = mytext
#     ),
#     alpha = 0.9
#   ) +
#   scale_size_continuous(range = c(4, 20)) +
#   scale_color_viridis_c(option = "plasma", na.value = "lightgrey") +
#   theme_void() +
#   theme(legend.position = "right")
# # 
# # Convertir a interactivo
# p_interactivo <- ggplotly(p, tooltip = "text")
# 
# p_interactivo





#GRÁFICA:Heatmap de correlaciones entre variables del agua y casos renales

# Seleccionar solo las columnas numéricas relevantes
datos_corr <- datos_combinados_final %>%
  select(
    swEcologicalStatus_promedio,
    swChemicalStatus_promedio,
    Area_total,
    total_casos_renales
  )

# Calcular matriz de correlaciones
matriz_corr <- round(cor(datos_corr, use = "pairwise.complete.obs"), 2)

# Pasamos a formato largo
matriz_corr_long <- as.data.frame(matriz_corr) %>%
  mutate(Var1 = rownames(.)) %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlacion")

# Heatmap
ggplot(matriz_corr_long, aes(x = Var1, y = Var2, fill = Correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlacion), color = "black", size = 5) +
  scale_fill_viridis_c(option = "plasma", limits = c(-1,1)) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    fill = "Correlación",
    title = "Heatmap de correlaciones entre calidad del agua y casos renales"
  )



