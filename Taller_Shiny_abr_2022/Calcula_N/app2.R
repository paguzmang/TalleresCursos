# abril 2022
# paguzmang
# Aplicacion Shiny para calcular el tamano
# de muestra para estimar la media poblacional


# Librerias
library(shiny)
library(tidyverse)


# Objeto UI -----
ui <- fluidPage(
  
  # Titulo de la app ----
  titlePanel("Calcule el tamaño de la muestra"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Texto libre ----
      "Aplicación para calcular el tamaño de la muestra para estimar la media de la poblacion con una confianza del 95% y un margen de error del 2%",
      
      br(),
      br(),
      
      # Input: Slider for the number of bins ----
      numericInput(inputId = "cv",
                  label = "Coeficiente de variación (%)",
                  min = 0,
                  max = 500, step = 5,
                  value = 20)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabla de resultados ----
      br(), 
      br(),
      br(),
      
      h2("Resultados"),
      
      tableOutput(outputId = "tablaRes")
      
    )
  )
)

# Funcion server -----
server <- function(input, output){
  
  output$tablaRes <- renderTable(
    
    align = "c", digits = 1, bordered = T,
    
    expr = {
    
    # Parametros
    conf <- 0.95
    z <- qnorm(p = 1-(1-conf)/2)
    pe <- 0.02
    cv <- input$cv/100
    
    # Calculo de n
    n <- (z * cv / pe)^2
    n <- ifelse(cv == 0, 1, n)
    
    # Data.frame resultante:
    tibble(
      "Confianza (%)"         = round(conf*100), 
      "Error relativo (%)"    = round(pe*100,1),
      "Coef.de variación (%)" = cv*100,
      "Tamaño de muestra (n)" = ceiling(n)
    )
  })
  
}

# Llamada al comando shinyApp ----
shinyApp(ui = ui, server = server)

