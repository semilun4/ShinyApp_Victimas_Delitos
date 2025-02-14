server <- function(input, output, session) {
  # Filtrar datos reactivos
  datos_filtrados <- reactive({
    data <- datos_limpios %>%
      filter(
        (input$entidad == "Todas" | Entidad == input$entidad),
        (input$tipo_delito == "Todos" | `Tipo.de.delito` == input$tipo_delito),
        (input$sexo == "Todos" | Sexo == input$sexo),
        Año == input$anio
      )
    data
  })
  
  # Vector de cambio de nombres para los delitos
  nombres_delitos <- c(
    "Aborto" = "Aborto",
    "Corrupción de menores" = "Corrupción menores",
    "Extorsión" = "Extorsión",
    "Feminicidio" = "Feminicidio",
    "Homicidio" = "Homicidio",
    "Lesiones" = "Lesiones",
    "Otros delitos contra la sociedad" = "Delitos sociedad",
    "Otros delitos que atentan contra la libertad personal" = "Delitos libertad",
    "Otros delitos que atentan contra la vida y la integridad corporal" = "Delitos integridad",
    "Rapto" = "Rapto",
    "Secuestro" = "Secuestro",
    "Tráfico de menores" = "Tráfico menores",
    "Trata de personas" = "Trata de personas"
  )
  
  # Gráfico de barras por mes
  output$grafico <- renderPlotly({
    req(datos_filtrados())
    data <- datos_filtrados() %>%
      group_by(Mes) %>%
      summarise(Delitos = sum(as.numeric(Delitos), na.rm = TRUE))
    
    plot <- ggplot(data, aes(x = Mes, y = Delitos, fill = Mes)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      scale_fill_manual(values = c(
        "#E5E5E5", "#e0ecf4", "#bfd3e6", "#9ebcda",
        "#8c96c6", "#8c6bb1", "#88419d", "#810f7c",
        "#4d004b", "#67001f", "#980043", "#ce1256"
      )) + 
      theme_minimal() +
      labs(title = "Distribución de delitos por mes", x = "Mes", y = "Número de delitos")
    
    ggplotly(plot)
  })
  
  # Gráfico de barras por género
  output$grafico_genero <- renderPlotly({
    req(datos_filtrados())
    data <- datos_filtrados() %>%
      group_by(Mes, Sexo) %>%
      summarise(Delitos = sum(as.numeric(Delitos), na.rm = TRUE))
    
    plot <- ggplot(data, aes(x = Mes, y = Delitos, fill = Sexo)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Mujer" = "#56008C", "Hombre" = "#8A84F4")) + # Colores personalizados
      theme_minimal() +
      labs(title = "Distribución de delitos por mes y sexo", x = "Mes", 
           y = "Número de delitos", fill = "Sexo")
    
    ggplotly(plot)
  })
  
  # Gráfico de barras apiladas por tipo de delito y sexo
  output$grafico_barras <- renderPlotly({
    req(datos_filtrados())
    
    # Agrupar los datos por tipo de delito y sexo, y sumar los delitos
    data <- datos_filtrados() %>%
      group_by(`Tipo.de.delito`, Sexo) %>%
      summarise(Delitos = sum(as.numeric(Delitos), na.rm = TRUE), .groups = "drop") %>%
      mutate(`Tipo.de.delito` = recode(`Tipo.de.delito`, !!!nombres_delitos))
    
    # Verificar que haya datos para graficar
    validate(
      need(nrow(data) > 0, "No hay datos disponibles para el gráfico de barras.")
    )
    
    # Crear el gráfico de barras apiladas
    plot <- ggplot(data, aes(x = `Tipo.de.delito`, y = Delitos, fill = Sexo)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c("Mujer" = "#56008C", "Hombre" = "#8A84F4")) + # Colores personalizados
      theme_minimal() +
      labs(
        title = "Delitos por tipo y sexo",
        x = "Tipo de delito",
        y = "Número de delitos",
        fill = "Sexo"
      ) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Rotar etiquetas
    
    # Convertir a gráfico interactivo con plotly
    ggplotly(plot)
  })
  
  # Tabla interactiva
  output$tabla <- renderDT({
    datatable(datos_filtrados(), extensions = "Buttons", options = list(
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      pageLength = 10
    ))
  })
}
