library(shiny)
library(plotly)
library(leaflet)
library(DT)

ui <- fluidPage(
  titlePanel("Análisis de Víctimas del Fuero Común en México"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("entidad", "Selecciona una entidad:", 
                  choices = c("Todas", unique(datos_limpios$Entidad))),
      selectInput("tipo_delito", "Selecciona un tipo de delito:", 
                  choices = c("Todos", unique(datos_limpios$`Tipo.de.delito`))),
      selectInput("sexo", "Selecciona el género:", 
                  choices = c("Todos", unique(datos_limpios$Sexo))),
      selectInput("anio", "Selecciona el año:", 
                  choices = unique(datos_limpios$Año))
    ),
    
    mainPanel(
      tabsetPanel(
        # Gráfico de barras por mes
        tabPanel("Distribución por Mes", plotlyOutput("grafico")),
        
        # Gráfico de barras por género
        tabPanel("Distribución por Género", plotlyOutput("grafico_genero")),
        
        # Gráfico por tipo de delito
        tabPanel("Delitos por Tipo y Sexo", plotlyOutput("grafico_barras")),
        
        # Tabla interactiva
        tabPanel("Tabla de Datos", DTOutput("tabla"))
      )
    )
  )
)
