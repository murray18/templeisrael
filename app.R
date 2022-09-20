library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(googlesheets4)
library(leaflet.extras)
library(htmltools)



df <- read_sheet('https://docs.google.com/spreadsheets/d/1XBoWe9cJXfxpmZ3i5E4HsxWBOrkQCoa1UU-ORNpQ-iI')

df$Type <- as.factor(df$Type)


icons <- iconList(
  synagogue = makeIcon("assets/synagogue.png", iconWidth=25, iconHeight=25),
  person = makeIcon("assets/person.png", iconWidth=25, iconHeight=25)
)

ui <- fluidPage(
  leafletOutput("mymap"),
  sidebarPanel(
    sliderInput("year", "Years", min(df$Year), max(df$Year),
                value = range(df$Year), step = 1
    ),
  ),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    df[df$Year >= input$year[1] & df$Year <= input$year[2],]
  })
  
  
  
  output$mymap <- renderLeaflet({
   leaflet() %>%
      addTiles() %>%
      fitBounds(min(df$Long), min(df$Lat), max(df$Long), max(df$Lat))
  })
  
  observe({
    data = filtered_data()
    leafletProxy("mymap", data = data) %>%
      clearMarkers() %>%
      addMarkers(~Long, ~Lat, label = ~htmlEscape(Name), icon = ~icons[Type])
  })
}

shinyApp(ui, server)