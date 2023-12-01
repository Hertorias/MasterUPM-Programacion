#Librerías
library(rvest)
library(stringr)
library(tidyverse)
library(ggplot2)

#Datos que se van a obtener

#Nombre de los países
trackNames <- c('bahrain', 'saudi-arabia', 'australia', 'azerbaijan', 'miami', 'monaco', 'spain', 'canada',
                'austria', 'great-britain', 'hungary', 'belgium', 'netherlands', 'italy', 'singapore', 'japan', 'qatar', 'united-states', 'mexico', 'brazil')

#ID identificativos para cada país
ids <- c('1141', '1142', '1143', '1207', '1208', '1210', '1211', '1212', '1213', '1214', '1215', '1216', '1217', '1218',
         '1219', '1220', '1221', '1222', '1223', '1224')

#Los datos que queremos obtener de la web
relevantDataNames <- c('race-result', 'fastest-laps', 'starting-grid', 'qualifying')

#Creamos dataframes vacíos a los cuales se les irá añadiendo la información
dfRaceResult <- data.frame()
dfFastestLaps <- data.frame()
dfStartingGrid <- data.frame()
dfQualifying <- data.frame()

#Variable contador para el scraper
contador <- 1

#Scraper
for (track in trackNames) {
  id <- ids[contador]
  for (i in relevantDataNames) {
    url <- paste('https://www.formula1.com/en/results.html/2023/races/', id, '/', track, '/', i, '.html', sep = "")
    rawData <- read_html(url)
    data_table <- html_table(rawData, fill = TRUE)[[1]]
    data_table$Driver <- str_replace_all(data_table$Driver, "[\n]+", " ")
    data_table$Driver <- str_replace_all(data_table$Driver, "[[:space:]]+", " ")
    data_table$DriverAbbr <- sapply(strsplit(data_table$Driver, " "), tail, 1)
    data_table$Track <- track
    if (i == 'race-result') {
      dfRaceResult <- rbind(dfRaceResult, data_table)
    } else if (i == 'fastest-laps') {
      dfFastestLaps <- rbind(dfFastestLaps, data_table)
    } else if (i == 'starting-grid') {
      dfStartingGrid <- rbind(dfStartingGrid, data_table)  
    } else {
      dfQualifying <- rbind(dfQualifying, data_table)
    }
  }
  contador <- contador+1
}
  
#Retoques finales a los dataframes que serán necesarios  
dfRaceResult <- dfRaceResult[, colnames(dfRaceResult) != ""]
dfRaceResult$Track[dfRaceResult$Track == "austria"] <- "autria"
dfRaceResult$Track <- factor(dfRaceResult$Track, levels = unique(dfRaceResult$Track))
dfFastestLaps <- dfFastestLaps[, colnames(dfFastestLaps) != ""]
dfQualifying <- dfQualifying[, colnames(dfQualifying) != ""]
dfQualifying$Track[dfQualifying$Track == "austria"] <- "autria"
dfStartingGrid <- dfStartingGrid[, colnames(dfStartingGrid) != ""]
dfStartingGrid$Track[dfStartingGrid$Track == "austria"] <- "autria"
dfStartingGrid$Track <- factor(dfStartingGrid$Track, levels = unique(dfStartingGrid$Track))

#Volcado de los datos a csv
#Se debe cambiar el PATH para que funcione correctamente
write.csv(dfRaceResult, "C:/Users/Hector/Desktop/TrabajoF1_vFinal/Formula1/RaceResults.csv", row.names = FALSE)
write.csv(dfFastestLaps, "C:/Users/Hector/Desktop/TrabajoF1_vFinal/Formula1/FastestLaps.csv", row.names = FALSE)
write.csv(dfQualifying, "C:/Users/Hector/Desktop/TrabajoF1_vFinal/Formula1/Qualifying.csv", row.names = FALSE)
write.csv(dfStartingGrid, "C:/Users/Hector/Desktop/TrabajoF1_vFinal/Formula1/StartingGrid.csv", row.names = FALSE)