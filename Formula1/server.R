#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#Carga de librerías necesarias
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)

shinyServer(function(input, output) {
  
  #####################################################################################################################################################
  ##### Tratamiento de los datos para el primer gráfico (Vuelta rápida por piloto) #####
  #####################################################################################################################################################

    #Lectura y procesamiento de los datos
    dataframe_formula1 <- read.csv2(file = "FastestLaps.csv", sep = ",")

    dfFastestLaps <- dataframe_formula1
    dataframe_formula1$Track <- factor(dataframe_formula1$Track, levels = unique(dataframe_formula1$Track))
    dataframe_formula1$TrackAbbr <- substr(dataframe_formula1$Track, 1, 3)
    dataframe_formula1$TrackAbbr <- factor(dataframe_formula1$TrackAbbr, levels = unique(dataframe_formula1$TrackAbbr))
    
    #Hago que la columna de tiempos pase de formato texto a formato numérico
    dataframe_formula1 <- transform(dataframe_formula1,
                                    TimeFormatted = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Time)))
  
    #Dataframe con las vueltas más rápidas
    dataframeOfFastest <- dataframe_formula1 %>%
      group_by(Track) %>%
      slice_min(TimeFormatted)
    
    #En el gráfico aparecerá en la leyenda "Vuelta más rápida" como nombre del piloto
    dataframeOfFastest$Driver <- "Fastest Lap FL"

    #Concatenamos ambos dataframes
    dataframe_formula1 <- rbind(dataframe_formula1, dataframeOfFastest)
    
    #Limpiamos la columna 'Driver' para tenerla sin la abreviación del nombre. Ejemplo: Fernando Alonso ALO ---> Fernando Alonso
    dataframe_formula1$Driver <- sub(" [^ ]*$", "", dataframe_formula1$Driver)
  
  #####################################################################################################################################################
  ##### Tratamiento de los datos para el segundo gráfico #####
  #####################################################################################################################################################

    #Lectura de los datos
    dfRaceResults <- read.csv2(file = "RaceResults.csv", sep = ",")

    dfRaceResults$Track <- factor(dfRaceResults$Track, levels = unique(dfRaceResults$Track))
    dfRaceResults$TrackAbbr <- substr(dfRaceResults$Track, 1, 3)
    dfRaceResults$TrackAbbr <- factor(dfRaceResults$TrackAbbr, levels = unique(dfRaceResults$TrackAbbr))

    #Creamos un dataframe con los puntos por cada escudería sumados en cada circuito
    dataframePuntosEscuderia <- aggregate(PTS ~ Car + Track, data = dfRaceResults, sum)

    #Transformaciones de los datos para mayor claridad visual en el gráfico
    dataframePuntosEscuderia$TrackAbbr <- substr(dataframePuntosEscuderia$Track, 1, 3)
    dataframePuntosEscuderia$TrackAbbr <- factor(dataframePuntosEscuderia$TrackAbbr, levels = unique(dataframePuntosEscuderia$TrackAbbr))

    #Limpieza de la columna con los nombres de las escuderías para que sean más cortos y reconocibles. Ejemplo: Aston Martin Aramco Mercedes ---> Aston Martin
    dataframePuntosEscuderia$Car <- gsub(" Renault", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Ferrari", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Mercedes", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Aramco Mercedes", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Aramco", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Renault", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" RBPT", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Racing", "", dataframePuntosEscuderia$Car)
    dataframePuntosEscuderia$Car <- gsub(" Honda", "", dataframePuntosEscuderia$Car)

    #Nombre de los circuitos para ordenarlos por cuando se corrió en el circuito, en vez de alfabéticamente
    trackNames <- c('bah', 'sau', 'aus', 'aze', 'mia', 'mon', 'spa', 'can',
                'aut', 'gre', 'hun', 'bel', 'net', 'ita', 'sin', 'jap', 'qat', 'uni', 'mex', 'bra')

    #Creamos un dataframe que reaccione a las opciones escogidas por el usuario
    dataframePuntosEscuderiaGrafico <- reactive({
      filter(dataframePuntosEscuderia, Car %in% input$checkbox_options)
    })

  #####################################################################################################################################################
  ##### Tratamiento de los datos para el tercer gráfico #####
  #####################################################################################################################################################

  #Lectura de datos
  dfQualifying <- read.csv2(file = "Qualifying.csv", sep = ",")

  #Tratamiento de los datos, quitamos la abreviatura de los nombres de los pilotos y cambiamos los tiempos a formato numérico
  dfQualifying$Driver <- sub(" [^ ]*$", "", dfQualifying$Driver)
  dfQualifying <- transform(dfQualifying,
                                Q3TimeFormat = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Q3)))
  dfQualifying <- transform(dfQualifying,
                                Q2TimeFormat = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Q2)))
  dfQualifying <- transform(dfQualifying,
                                Q1TimeFormat = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Q1)))

  #Filtramos para obtener aquellos pilotos cuya posición en clasificación es 1 (pole)
  dfPolePositions <- filter(dfQualifying, dfQualifying$Pos==1)
  dfPolePositions$TrackAbbr <- substr(dfPolePositions$Track, 1, 3)
  dfPolePositions$TrackAbbr <- factor(dfPolePositions$TrackAbbr, levels = unique(dfPolePositions$TrackAbbr))
  #dfPolePositions <- transform(dfPolePositions,
   #                               Q3TimeFormat = as.numeric(sub("(\\d+):(\\d+)\\.(\\d+)", "\\1.\\2\\3", Q3)))

  #####################################################################################################################################################
  ##### Tratamiento de los datos para el cuarto gráfico #####
  #####################################################################################################################################################

  #Cambiamos a formato numérico la velocidad media y la convertimos a km/h, frente a mph
  dfFastestLaps$Avg.Speed <- as.numeric(dfFastestLaps$Avg.Speed)
  dfFastestLaps$AverageSpeedFormatted <- dfFastestLaps$Avg.Speed * 1.60934

  #Creamos un dataframe que reaccione al input del usuario (en este caso, selección del circuito)
  datosFiltrados <- reactive({
    filter(dfFastestLaps, Track == input$track)
  })

  #Reordenamos el dataframe para que las velocidades sean de mayor a menor en el gráfico
  datosFiltradosOrdenados <- reactive({
    dataTemporal <- datosFiltrados()
    dataTemporal$DriverAbbr <- reorder(dataTemporal$DriverAbbr, -dataTemporal$AverageSpeedFormatted)
    dataTemporal
  })

  #####################################################################################################################################################
  ##### Tratamiento de los datos para el quinto gráfico #####
  #####################################################################################################################################################

  #Filtramos para quedarnos con aquellas vueltas más rápidas
  dfLaps <- filter(dfFastestLaps, dfFastestLaps$Pos==1)

  #####################################################################################################################################################
  ##### Tratamiento de los datos para el sexto gráfico #####
  #####################################################################################################################################################

  #Filtramos para quedarnos con aquellos datos en los que se consigue la victoria
  dfVictories <- filter(dfRaceResults, dfRaceResults$Pos==1)

  #####################################################################################################################################################
  ##### Tratamiento de los datos para el septimo gráfico #####
  #####################################################################################################################################################

  #Lectura de los datos
  dfStartingGrid <- read.csv2(file = "StartingGrid.csv", sep = ",")

  #Tratamiento de los datos, haciendo factores haremos que se respete el orden de los circuitos según se corrió en ellos, en vez de orden alfabético
  dfStartingGrid$Track <- factor(dfStartingGrid$Track, levels = unique(dfStartingGrid$Track))
  dfStartingGrid$TrackAbbr <- substr(dfStartingGrid$Track, 1, 3)
  dfStartingGrid$TrackAbbr <- factor(dfStartingGrid$TrackAbbr, levels = unique(dfStartingGrid$TrackAbbr))

  #Seleccionamos las columnas necesarias para nuestro gráfico
  dfRaceResultsTemp <- select(dfRaceResults, Pos, DriverAbbr, TrackAbbr, Track)
  dfStartingGridTemp <- select(dfStartingGrid, Pos, DriverAbbr, TrackAbbr, Track)

  #Renombramos la columna 'pos' en ambos dataframes para tener mejor idea sobre lo que nos refererimos
  colnames(dfStartingGridTemp)[1] <- 'StartPos'
  colnames(dfRaceResultsTemp)[1] <- 'EndPos'

  #Combinamos ambos dataframes
  dfToPlot <- merge(dfStartingGridTemp, dfRaceResultsTemp, by = c('TrackAbbr', 'DriverAbbr', 'Track'))

  #Creamos un dataframe que reaccione al input del usuario
  dfToPlotReactive <- reactive({
    filter(dfToPlot, Track == input$track2)
  })

  #####################################################################################################################################################
  
    #Definimos el tema de los gráficos
    my_theme <- theme_minimal() + 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
    )
    
    #Gráfico de las Vueltas rápidas de cada piloto, y poder compararlas con otro piloto o con la vuelta más rápida realizada independientemente del piloto
    output$formula1_plot_fastest_laps <- renderPlot({

        grafico <-
        ggplot()+
        geom_line(size = 1.2, data = dataframe_formula1[dataframe_formula1$Driver == input$driver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) + 
        geom_point(size = 2, data = dataframe_formula1[dataframe_formula1$Driver == input$driver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) +
        scale_color_manual(values = c('Fernando Alonso' = '#2D826D', 'Lance Stroll' = '#194a3e', 'Carlos Sainz' = '#ED1C24', 'Charles Leclerc' = '#cc040c', 'Max Verstappen' = '#1E5BC6', 'Sergio Perez' = '#023896',
        'Lewis Hamilton' = '#6CD3BF', 'George Russell' = '#06bd99', 'Lando Norris' = '#F58020', 'Oscar Piastri' = '#b35204', 'Alexander Albon' = '#37BEDD', 'Logan Sargeant' = '#18859e', 'Liam Lawson' = '#1b2b36',
        'Esteban Ocon' = '#2293D1', 'Pierre Gasly' = '#0272b0', 'Yuki Tsunoda' = '#4E7C9B', 'Daniel Ricciardo' = '#335063', 'Kevin Magnussen' = '#8C8F91', 'Nico Hulkenberg' = '#4f5152',
        'Valtteri Bottas' = '#B12039', 'Guanyu Zhou' = '#ad0724', 'Nyck De Vries' = '#19252e', 'Fastest Lap' = '#1cfc03')) +   #03a329
        scale_y_reverse() + 
        labs(x = "Circuito", y = "Tiempo de vuelta rápida",
             title = "Pilotos con su vuelta rápida en cada circuito") +
        my_theme
      
      #Condiciones que determinan si se muestra a otro piloto y/o se muestra la vuelta rápida
       if (input$selectsecondcountry == TRUE & input$selectfastestlap == TRUE)
         grafico+
          geom_line(size = 1.2, data = dataframe_formula1[dataframe_formula1$Driver == input$secondDriver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataframe_formula1[dataframe_formula1$Driver == input$secondDriver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) +
          geom_line(size = 1.2, data = dataframe_formula1[dataframe_formula1$Driver == "Fastest Lap", ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataframe_formula1[dataframe_formula1$Driver == "Fastest Lap", ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) 
       
        else if (input$selectsecondcountry == TRUE & input$selectfastestlap == FALSE)
          grafico +
          geom_line(size = 1.2, data = dataframe_formula1[dataframe_formula1$Driver == input$secondDriver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataframe_formula1[dataframe_formula1$Driver == input$secondDriver, ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver))
        
        else if (input$selectsecondcountry == FALSE & input$selectfastestlap == TRUE)
          grafico +
          geom_line(size = 1.2, data = dataframe_formula1[dataframe_formula1$Driver == "Fastest Lap", ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) + 
          geom_point(size = 2, data = dataframe_formula1[dataframe_formula1$Driver == "Fastest Lap", ], aes(x = TrackAbbr, y = TimeFormatted,  color=Driver, group = Driver)) 
        
       else grafico

    })

    #Gráfico de la Tendencia de los puntos que obtiene cada escudería a lo largo del campeonato

    output$formula1_plot_points <- renderPlot({

      dfPrueba <- dataframePuntosEscuderiaGrafico()

      grafico2 <-
      ggplot(dfPrueba, aes(x = factor(TrackAbbr, levels = trackNames), y = PTS, group = Car, color = Car)) + 
      geom_smooth(se = FALSE, size=1.2) +
      labs(x = "Circuito", y = "Puntos obtenidos",
          title = "Tendencia de los puntos obtenidos a lo largo del campeonato por escudería") + my_theme +
      scale_color_manual(values = c('Aston Martin' = '#2D826D', 'Ferrari' = '#ED1C24', 'Red Bull' = '#1E5BC6', 'Mercedes' = '#6CD3BF', 'McLaren' = '#F58020', 'Williams' = '#37BEDD',
      'Alpine' = '#2293D1', 'AlphaTauri' = '#4E7C9B', 'Haas' = '#8C8F91', 'Alfa Romeo' = '#B12039'))

      grafico2
    })

    #Gráfico del Número de poles que ha realizado cada piloto

    output$formula1_plot_poles <- renderPlot({
      grafico3 <- ggplot(data=dfPolePositions, aes(fct_infreq(DriverAbbr), fill=DriverAbbr))+
      geom_bar() + 
      labs(x = "Piloto", y = "Número de poles",
            title = "Número de poles que ha hecho cada piloto (mínimo 1 pole)") +
      scale_fill_manual(values = c('SAI' = '#ED1C24', 'LEC' = '#cc040c', 'VER' = '#1E5BC6', 'PER' = '#023896', 'HAM' = '#6CD3BF')) +
      scale_y_continuous(limits = c(0,23)) +
      my_theme

      grafico3
    })

    #Gráfico de la Velocidad media de cada piloto

    output$formula1_plot_average_speed <- renderPlot({

      datosFiltrados <- datosFiltrados()

      grafico4 <- ggplot(datosFiltradosOrdenados(), aes(x = DriverAbbr, y = AverageSpeedFormatted, fill=DriverAbbr)) +
      geom_col()+
      labs(x = "Piloto", y = "Velocidad Media (km/h)", title = "Velocidad Media de Cada Piloto en Vuelta Rápida") +
      scale_fill_manual(values = c('ALO' = '#2D826D', 'STR' = '#194a3e', 'SAI' = '#ED1C24', 'LEC' = '#cc040c', 'VER' = '#1E5BC6', 'PER' = '#023896',
        'HAM' = '#6CD3BF', 'RUS' = '#06bd99', 'NOR' = '#F58020', 'PIA' = '#b35204', 'ALB' = '#37BEDD', 'SAR' = '#18859e',
        'OCO' = '#2293D1', 'GAS' = '#0272b0', 'TSU' = '#4E7C9B', 'RIC' = '#335063', 'MAG' = '#8C8F91', 'HUL' = '#4f5152',
        'BOT' = '#B12039', 'ZHO' = '#ad0724', 'LAW' = '#1b2b36', 'DEV' = '#19252e'))+
        coord_cartesian(ylim=c(160,input$slider1), expand=TRUE) +
        scale_y_continuous(breaks = seq(160, input$slider1, input$slider2))+
      my_theme  

      grafico4

    })

    #Gráfico sobre el Número de vueltas rápidas que ha marcado cada piloto

    output$formula1_plot_number_of_fastest_laps <- renderPlot({
      grafico5 <- ggplot(data=dfLaps, aes(fct_infreq(DriverAbbr), fill=DriverAbbr))+
      geom_bar() + 
      labs(x = "Piloto", y = "Número de Vueltas Rápidas",
            title = "Número de vueltas rápidas que ha marcado cada piloto (mínimo 1 vuelta rápida)") +
      scale_fill_manual(values = c('ALO' = '#2D826D', 'VER' = '#1E5BC6', 'PER' = '#023896', 'HAM' = '#6CD3BF', 'PIA' = '#b35204', 
                                   'RUS' = '#06bd99', 'ZHO' = '#ad0724', 'NOR' = '#F58020', 'TSU' = '#4E7C9B')) +
      coord_flip() +
      my_theme

      grafico5
    })

    #Gráfico del Número de victorias que ha conseguido cada piloto

    output$formula1_plot_victories <- renderPlot({
      grafico6 <- ggplot(data=dfVictories, aes(fct_infreq(DriverAbbr), fill=DriverAbbr))+
      geom_bar() + 
      labs(x = "Piloto", y = "Número de Victorias",
            title = "Número de Victorias por Piloto (Mínimo 1 Victoria)") +
      scale_fill_manual(values = c('SAI' = '#ED1C24', 'VER' = '#1E5BC6', 'PER' = '#023896')) +
      my_theme

      grafico6
    })

    #Gráfico de la Comparación entre cómo clasifica y termina la carrera cada piloto

    output$formula1_plot_qualifying_vs_race <- renderPlot({

      grafico7 <- ggplot(data = dfToPlotReactive(), aes(x = DriverAbbr, group = TrackAbbr)) +
      geom_line(aes(y = EndPos, color = "Carrera"), size = 1.2) +
      geom_point(aes(y = EndPos, color = "Carrera"), size = 3, shape = 16) +
      geom_line(aes(y = StartPos, color = "Clasificación"), size = 1.2) +
      geom_point(aes(y = StartPos, color = "Clasificación"), size = 3, shape = 16) +
      scale_color_manual(values = c("lightblue", "lightcoral"), name = "Tipo de Posición",
                        labels = c("Carrera", "Clasficación")) +
      scale_y_discrete(limits = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "NC")) +
      labs(x = "Piloto", y = "Posición",
          title = "Posición en Carrera VS Posición en Clasifición de los Pilotos en Cada Circuito") +
      my_theme

      grafico7
    })
})