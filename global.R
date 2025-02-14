library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

DATADIR <- "C:/Users/semiramis/Documents/Me/INMUJERES/data/"
file <- paste0(DATADIR, "IDVFC_NM_dic24.csv")
datos <- fread(file, encoding = "Latin-1")
colnames(datos) <- iconv(colnames(datos), from = "Latin1", to = "UTF-8")

# Normalizar contenido del data
datos <- datos %>%
  mutate(across(everything(), ~ iconv(., from = "Latin1", to = "UTF-8")))

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
           "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

datos <- datos %>%
  mutate(across(all_of(meses), ~ as.numeric(gsub("[^0-9.]", "", .)))) 

datos_limpios <- datos %>% 
  mutate(Delitos = rowSums(select(., all_of(meses)), na.rm = TRUE))

duplicated_columns <- names(datos_limpios)[duplicated(names(datos_limpios))]
print(duplicated_columns)
names(datos_limpios) <- make.names(names(datos_limpios), unique = TRUE)

print(names(datos_limpios))

datos_limpios <- datos_limpios %>%
  rename(Delitos_totales = Delitos) 

datos_limpios <- datos_limpios %>%
  pivot_longer(
    cols = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
             "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"),
    names_to = "Mes",
    values_to = "Delitos"
  ) %>%
  mutate(Mes = factor(Mes, levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
                                      "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")))

str(datos_limpios)  
head(datos_limpios) 
