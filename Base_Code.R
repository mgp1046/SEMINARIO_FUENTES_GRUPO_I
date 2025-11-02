library(tidyverse)
library(rjson)

data_nef_url <- "https://ec.europa.eu/eurostat/api/dissemination/statistics/1.0/data/hlth_co_ren?format=JSON&unit=NR&unit=P_HTHAB&icd9cm=CM3995&icd9cm=CM3995_556&icd9cm=CM556&lang=EN"
datos_nefro <- fromJSON(file = data_nef_url)

view(datos_nefro)
str(datos_nefro)                        
