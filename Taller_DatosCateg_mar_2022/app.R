


# librerias
library(tidyverse)
library(shiny)

# Datos ----
dat <- read.csv(file = "datos/tratSimul.csv")

# Se modifica la tabla ----
dat1 <- dat %>%
  mutate(
    Edad   = cut(Age, br = c(20,40,60,80), labels = c("20-40","40-60","60-80")),
    Genero = fct_recode(Sex, Mujer = "Female", Hombre = "Male"),
    Trat   = fct_recode(Treatment, Tratado = "Treated"),
    Mejora = factor(Improved, levels = c("None", "Low", "Medium", "High"),
                    labels = c("Ninguna", "Baja", "Media", "Alta")),
  ) %>% select(-(Treatment:Improved))


# Seleccion del usuario ----
nomv <- data.frame(
  op = c("Edad", "Género", "Tratamiento"),
  i  = c(2,3,4) 
)

# Objeto ui ----
ui <- fluidPage(
  # App title ----
  titlePanel("Respuesta categórica"),
  sidebarLayout(
    sidebarPanel(
      p("Explore el comportamiento de las frecuencias asociadas a un variable categórica ordinal en función de una o dos variables explicativas"),
      br(),
      selectInput("r1", label = "Seleccione una variable para el eje X del gráfico", 
                  choices = nomv$op, multiple = F),
      selectInput("r2", label = "Seleccione una variable para dividir el gráfico", 
                  choices = c(nomv$op, "Ninguna"), 
                  selected = "Ninguna", multiple = F)
  ),
  mainPanel(
    plotOutput(outputId = "migrafico", width = "80%", height = "250px")
  ))
  )

# Objeto server ----
server <- function(input, output){
  
  output$migrafico <- renderPlot({
    r1 <- nomv$i[nomv$op == input$r1]
    r2 <- ifelse(input$r2 == "Ninguna" | input$r2 == input$r1, 0, 
                 nomv$i[nomv$op == input$r2])
    if(r2 == 0){
      # Grafico si r2 = Ninguna
      dat1f <- select(dat1, r1, 5 )
      names(dat1f)[1] <- "x"
      table(dat1f) %>%
        prop.table(margin = 1) %>%
        as.data.frame() %>%
        ggplot(aes(x = x, y = Freq*100, fill = fct_rev(Mejora))) + 
        geom_col() +
        scale_fill_brewer(direction = -1, palette = "YlGn") +
        labs(x = nomv$op[nomv$i == r1], y = "% de Pacientes", 
             fill = "Mejora en\nlos síntomas")
    } else{
      # Grafico si r2 != Ninguna
      dat1f <- select(dat1, r1, 5, r2 )
      names(dat1f)[c(1,3)] <- c("x1", "x2")
      table(dat1f) %>%
        prop.table(margin = c(1,3)) %>%
        as.data.frame() %>%
        ggplot(aes(x = x1, y = Freq*100, fill = fct_rev(Mejora))) + 
        geom_col() +
        scale_fill_brewer(direction = -1, palette = "YlGn") +
        facet_grid(~ x2) +
        labs(x = nomv$op[nomv$i == r1], y = "% de Pacientes", 
             fill = "Mejora en\nlos síntomas")
    }
  })

}

# Llamada a shinyApp ----
shinyApp(ui = ui, server = server)



