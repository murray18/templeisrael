library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(googlesheets4)
library(leaflet.extras)
library(htmltools)

googlesheets4::gs4_auth()

df <- read_sheet('https://docs.google.com/spreadsheets/d/1XBoWe9cJXfxpmZ3i5E4HsxWBOrkQCoa1UU-ORNpQ-iI')

df$Type <- as.factor(df$Type)

# Attribution to https://www.freepik.com
# https://www.flaticon.com/authors/hqrloveq
# https://www.flaticon.com/search/2?word=store&order_by=4
icons <- iconList(
  synagogue = makeIcon("assets/icons/synagogue_4.png", iconWidth=30, iconHeight=30, className='synagogue'),
  person = makeIcon("assets/icons/home.png", iconWidth=25, iconHeight=25),
  store = makeIcon("assets/icons/store.png", iconWidth=25, iconHeight=25)
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
  sidebarPanel(
    textOutput("population"),
    textOutput("stores"),
    textOutput("synagogues")
   )
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
  }
  
  output$population <- renderText({
    data = filtered_data()
    paste0("Total Jewish Population: ", nrow(data[data$Type == 'person',]))
  })
  
  output$stores <- renderText({
    data = filtered_data()
    paste0("Total Jewish Stores: ", nrow(data[data$Type == 'store',]))
  })
  
  output$synagogues <- renderText({
    data = filtered_data()
    paste0("Total Synagogues: ", nrow(data[data$Type == 'synagogue',]))
  })
  
  
  output$mymap <- renderLeaflet({
   leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
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