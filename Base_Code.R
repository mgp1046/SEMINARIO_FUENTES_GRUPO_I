library(eurostat)
library(xml2)


datos_nef_valores <- get_eurostat(search_eurostat("renal")[2], lang = "en")

datos_nef_codigos <- get_eurostat(search_eurostat("renal")[2], lang = "en", type = "label")

str(datos_nef_valores)
str(datos_nef_codigos)

