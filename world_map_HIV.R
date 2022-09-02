library(rgdal)
library(RColorBrewer)
library(dplyr)
library(geojson)
library(ggplot2)
library(ggrepel)
library(leaflet)
library(shiny)
library(tidyverse)
library(tidyr)
library(stringr)
library(sf)

world_woman <- read.csv("world_woman_HIV.csv")
world_spdf <- geojsonio::geojson_read("countries.geojson", what = "sp")

server <- function(input,output,session){
  
  # add data to map
  data <- reactive({
    req(input$year)
    comb <-merge(world_spdf, world_woman[which(world_woman$Year == input$year), ], by.x = "ADMIN", by.y = "Country", all.x = TRUE)
  })
  # create interactive map
  output$map <-  renderLeaflet({ 
    # Create a color palette with handmade bins.
    mybins <- c(0,10,20,30,40,50,60, 70,100)
    mypalette <- colorBin( palette="RdPu", domain=data()$Percentage, na.color="transparent", bins=mybins)
    # Prepare the text for tooltips:
    mytext = sprintf(
      "<strong>%s</strong><br/>%g percent of HIV cases are women",
      data()$ADMIN, data()$Percentage
    ) %>% lapply(htmltools::HTML)
    # Final Map
    leaflet(data()) %>% 
      addTiles()  %>% 
      setView( lat=10, lng=0 , zoom=2) %>%
      addPolygons( 
        fillColor = ~mypalette(Percentage), 
        stroke=TRUE, 
        fillOpacity = 0.9, 
        color="white", 
        weight=0.3,
        label = mytext,
        labelOptions = labelOptions( 
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "13px", 
          direction = "auto"
        )
      ) %>%
      addLegend( pal=mypalette, values=~Percentage, opacity=0.9, title = "Proportion", position = "bottomleft" )
  })
  observe
}

ui <- fluidPage(
  mainPanel(selectInput(inputId="year",
                        label = "Select a year",
                        unique(world_woman$Year))),
  
  leafletOutput(outputId = "map", height = 500)
)

shinyApp(ui, server)
