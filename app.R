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
  synagogue = makeIcon("assets/synagogue-solid.svg", iconWidth=25, iconHeight=25, className='synagogue'),
  person = makeIcon("assets/person.png", iconWidth=25, iconHeight=25),
  store = makeIcon("assets/store.jpeg", iconWidth=25, iconHeight=25)
)

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  leafletOutput("mymap"),
  sidebarPanel(
    sliderInput("year", "Years", min(df$Year), max(df$Year),
                value = range(df$Year), step = 1, sep = ""
    ),
    selectInput(
      "occupation",
      "Occupation",
      unique(df$Occupation),
      selected = NULL,
      multiple = TRUE,
      selectize = TRUE,
    )
  ),
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    data = df
    if(!is.null(input$year)) {
      data = data[(data$Year >= input$year[1] & df$Year <= input$year[2]),]
    }
    if(!is.null(input$occupation)) {
      data = data[data$Occupation %in% input$occupation,]
    }
    data
  })
  
  get_label_html <- function(Name, Occupation) {
    lapply(paste0(Name, "<br>", Occupation), htmltools::HTML)
    #HTML(paste0("<br>", Name,"<br> Occupation: ", Name))
  }
  
  
  output$mymap <- renderLeaflet({
   leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      fitBounds(min(df$Long), min(df$Lat), max(df$Long), max(df$Lat))
  })
  
  observe({
    data = filtered_data()
    leafletProxy("mymap", data = data) %>%
      clearMarkers() %>%
      addMarkers(~Long, ~Lat, label = ~get_label_html(Name, Occupation), icon = ~icons[Type])
  })
}

shinyApp(ui, server)