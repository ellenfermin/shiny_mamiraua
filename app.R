rm(list = ls())
# Carregar pacotes necessários
library(shiny)
library(sf)
library(dplyr)
library(raster)
library(leaflet)
library(rsconnect)

# Definir o diretório de trabalho como o local do script
setwd("C:/Users/ellen/Desktop/mamiraua/shiny")
getwd()
dir()
# Carregar os dados de queimadas e raster usando caminhos relativos
burn_data <- st_read("amz_public.shp")  # Caminho relativo ###testar novos dados diminuidos...
jaguar_raster <- raster("jaguar_prob.tif")  # Caminho relativo
dir()

# Pre-processamento dos dados
burn_data$VIEW_DATE <- as.Date(burn_data$VIEW_DATE)
burn_data_2020 <- burn_data %>%
  filter(format(VIEW_DATE, "%Y") == "2020" & format(VIEW_DATE, "%m") == "06") %>%
  st_make_valid()

centroides <- st_centroid(burn_data_2020)
centroides$Jaguar_Prob <- extract(jaguar_raster, st_coordinates(centroides))

# UI do Shiny
ui <- fluidPage(
  titlePanel("Mapa de Queimadas e Probabilidade de Jaguar - Junho de 2020"),
  leafletOutput("mapa", height = "700px")  # Altura ajustada para o mapa
)

# Servidor do Shiny
server <- function(input, output, session) {
  output$mapa <- renderLeaflet({
    leaflet(data = centroides) %>%
      addTiles() %>%
      addCircleMarkers(
        ~st_coordinates(centroides)[,1], ~st_coordinates(centroides)[,2],
        color = ~colorNumeric("viridis", Jaguar_Prob)(Jaguar_Prob),
        radius = 5,
        popup = ~paste("ID:", FID, "<br>Probabilidade de Jaguar:", round(Jaguar_Prob, 2)),
        fillOpacity = 0.7
      ) %>%
      addLegend(
        position = "bottomright", 
        pal = colorNumeric("viridis", centroides$Jaguar_Prob),
        values = centroides$Jaguar_Prob,
        title = "Probabilidade de Jaguar ",
        labFormat = labelFormat(digits = 2),
        opacity = 1
      )
  })
}

##Executar o app Shiny
shinyApp(ui = ui, server = server)

##Comando para rodar localmente, mas não no código
runApp("C:/Users/ellen/Desktop/mamiraua/shiny")

# Para deploy em ShinyApps.io, use:
rsconnect::deployApp("C:/Users/ellen/Desktop/mamiraua/shiny")



