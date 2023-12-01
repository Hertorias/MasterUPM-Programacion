#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(
  
  fluidPage(

    #Titulo de la pagina
    titlePanel("Análisis de la temporada 2023 de Fórmula 1"),
    
    #Panel con las opciones ed gráficos
    mainPanel(
      tabsetPanel(id="formula1_tab",
        tabPanel("Puntos por escudería", plotOutput("formula1_plot_points")),
        tabPanel("Poles por piloto", plotOutput("formula1_plot_poles")),
        tabPanel("Victorias por piloto", plotOutput("formula1_plot_victories")),
        tabPanel("Vueltas rápidas por piloto", plotOutput("formula1_plot_number_of_fastest_laps")),
        tabPanel("Vueltas rápidas",plotOutput("formula1_plot_fastest_laps")),
        tabPanel("Velocidad media por piloto", plotOutput("formula1_plot_average_speed")),
        tabPanel("Clasificación vs carrera", plotOutput("formula1_plot_qualifying_vs_race"))
      )),

    #Menú para seleccionar al primer piloto
    conditionalPanel(
      condition = 'input.formula1_tab == "Vueltas rápidas"',
      fluidRow(
        column(7, 
          wellPanel(selectInput(inputId = "driver",
            label = "Selecciona el primer piloto:",
            choices = sort(c("Charles Leclerc", "Carlos Sainz", "Lewis Hamilton", "George Russell", "Kevin Magnussen", 
                              "Valtteri Bottas", "Esteban Ocon", "Yuki Tsunoda", "Fernando Alonso", "Guanyu Zhou", 
                              "Nico Hulkenberg", "Lance Stroll", "Alexander Albon", "Daniel Ricciardo", "Lando Norris", 
                              "Oscar Piastri", "Logan Sargeant", "Pierre Gasly", "Max Verstappen", "Sergio Perez", "Liam Lawson", "Nyck De Vries")),
            selected = "Fernando Alonso")
    )))),

    #Opciones de seleccion de otro piloto y/o de vuelta rápida
    conditionalPanel(
      condition = 'input.formula1_tab == "Vueltas rápidas"',
      column(5,
        fluidRow(checkboxInput("selectsecondcountry",label = "¿Seleccionar un piloto adicional?")),
        fluidRow(checkboxInput("selectfastestlap",label = "¿Ver las vueltas más rápidas?"))
    )),

    #Menú para seleccionar al segundo piloto
    conditionalPanel(
    condition = 'input.formula1_tab == "Vueltas rápidas"',
    fluidRow(
      column(7,
        conditionalPanel("input.selectsecondcountry == true",
          wellPanel(
            selectInput(inputId = "secondDriver",
              label = "Selecciona el segundo piloto:",
              choices = sort(c("Charles Leclerc", "Carlos Sainz", "Lewis Hamilton", "George Russell", "Kevin Magnussen", 
                                "Valtteri Bottas", "Esteban Ocon", "Yuki Tsunoda", "Fernando Alonso", "Guanyu Zhou", 
                                "Nico Hulkenberg", "Lance Stroll", "Alexander Albon", "Daniel Ricciardo", "Lando Norris", 
                                "Oscar Piastri", "Logan Sargeant", "Pierre Gasly", "Max Verstappen", "Sergio Perez", "Liam Lawson", "Nyck De Vries")),
              selected = "Carlos Sainz")
    ))))),

    conditionalPanel(
      condition = 'input.formula1_tab == "Puntos por escudería"',
      checkboxGroupInput(inputId = "checkbox_options",
        label = "Selecciona la escudería:",
        choices = c("Aston Martin", "Ferrari", "McLaren", "Red Bull", "Mercedes", "Alfa Romeo", "Haas", "Williams", "AlphaTauri", "Alpine"),
        selected = "Aston Martin")
    ),
    
    #Selección de circuito para velocidad media
    conditionalPanel(
      condition = 'input.formula1_tab == "Velocidad media por piloto"',
      fluidRow(
        column(7, 
          wellPanel(selectInput(inputId = "track",
            label = "Selecciona el circuito:",
            choices = c('bahrain', 'saudi-arabia', 'australia', 'azerbaijan', 'miami', 'monaco', 'spain', 'canada',
                        'austria', 'great-britain', 'hungary', 'belgium', 'netherlands', 'italy', 'singapore', 'japan', 'qatar', 'united-states', 'mexico', 'brazil'),
            selected = "bahrain")
    )))),

    #Slider para seleccionar la velocidad máxima a representar
    conditionalPanel(
      condition = 'input.formula1_tab == "Velocidad media por piloto"',
      fluidRow(
        column(9, 
          sliderInput("slider1", "Velocidad máxima a mostrar:", min = 250, max = 400, value = 380, step = 5)
    ))),

    #Slider para seleccionar los breaks en el gráfico para la velocidad
    conditionalPanel(
      condition = 'input.formula1_tab == "Velocidad media por piloto"',
      fluidRow(
        column(11, 
          sliderInput("slider2", "Breaks de velocidad:", min = 10, max = 50, value = 20, step = 5)
    ))),

    #Selección de circuito
    conditionalPanel(
      condition = 'input.formula1_tab == "Clasificación vs carrera"',
      fluidRow(
        column(7, 
          wellPanel(selectInput(inputId = "track2",
            label = "Selecciona el circuito:",
            choices = c('bahrain', 'saudi-arabia', 'australia', 'azerbaijan', 'miami', 'monaco', 'spain', 'canada',
            'autria', 'great-britain', 'hungary', 'belgium', 'netherlands', 'italy', 'singapore', 'japan', 'qatar', 'united-states', 'mexico', 'brazil'),
            selected = "bahrain")
    ))))
  )
)
