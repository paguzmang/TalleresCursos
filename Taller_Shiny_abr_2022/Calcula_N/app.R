# abril 2022
# paguzmang
# Aplicacion Shiny para calcular el tamano
# de muestra para estimar la media poblacional


# Librerias
library(shiny)
library(tidyverse)


# Objeto UI -----
ui <- fluidPage(
  
      # Titulo
      titlePanel("Cálculo del tamaño de la muestra"),
      
      # Texto libre ----
      p("Aplicación para calcular el tamaño de la muestra para estimar la media de la poblacion con una confianza del 95% y un margen de error del 2%. Sólo debe definir el coeficiente de variación de la variable bajo estudio"),
      
      br(),  # salto de linea
      #br(),
  
      # Input: Entrada numerica del CV ----
      numericInput(inputId = "cv",
                  label = "Coeficiente de variación (%)",
                  min = 0,
                  max = 500, step = 5,
                  value = 20),
      
      br(),
      br(),
      
      # Titulo para la tabla de resultados
      h2("Resultados"),
      
      # Se definen el tipo de resultado con un id
      tableOutput(outputId = "tablaRes")
)

# Funcion server -----
server <- function(input, output){
  
  output$tablaRes <- renderTable(
    
    digits = 1, bordered = T,
    
    expr = {
    
    # Parametros
    conf <- 0.95
    z <- qnorm(p = 1-(1-conf)/2)
    er <- 0.02
    cv <- input$cv/100
    
    # Calculo de n
    n <- (z * cv / er)^2
    n <- ifelse(cv == 0, 1, n)
    
    # Data.frame resultante:
    tibble(
      "Confianza (%)"         = round(conf*100), 
      "Error relativo (%)"    = round(er*100,1),
      "Coef.de variación (%)" = cv*100,
      "Tamaño de muestra (n)" = ceiling(n)
    )
  })
  
}

# Llamada al comando shinyApp ----
shinyApp(ui = ui, server = server)

