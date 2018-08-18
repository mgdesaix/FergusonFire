#######################################################
################### Ferguson Fire in Shiny ######################


library(leaflet)
library(raster)
library(ggplot2)
library(rgdal)
library(magrittr)
library(rsconnect)

reduced.sh <- readRDS("reduced.sh.RDS")
reduced.sh$id <- c(1:length(reduced.sh))
fire.table <- read.csv("FergusonFire.csv")


ui <- fluidPage(
  titlePanel("2018 Ferguson Fire"),
  p("A big thanks to the firefighters out there keeping this from spreading faster. Data from table collected from Cal Fire, shapefile data from the Geospatial Multi-Agency Coordination:"),
  uiOutput("url"),
  br(),
  p(strong("Press play beneath the slider to watch the daily spread of the fire")),
  fluidRow(
    leafletOutput("fire"),
    absolutePanel(draggable = T,
                  sliderInput("range",
                              "Date:",
                              min = min(reduced.sh$date),
                              max = max(reduced.sh$date),
                              value = min(reduced.sh$date),
                              animate = TRUE)),
    br(),
    br(),
    br(),
    br(),
    br(),
    tableOutput("firetable") 
  )
  
)

server <- function(input, output){
  url <- a("GeoMAC", href = "https://www.geomac.gov/")
  output$url <- renderUI({
    tagList(url)
  })
  output$firetable <- renderTable({
    fire.table
  })
  
  poly1 <- reactive({
    max.id <- reduced.sh[reduced.sh$date == input$range[1],]$id
    reduced.sh[c(1:max.id),]
  })
  output$fire <- renderLeaflet({
    leaflet(reduced.sh) %>%
      addTiles() %>%
      addPolygons()
  })
  observe({
    leafletProxy("fire", data = poly1()) %>%
      clearShapes() %>%
      addPolygons(fillColor = "red")
  })
}

shinyApp(ui,server)