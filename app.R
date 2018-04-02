library(tidyverse)
library(leaflet)
library(shiny)
library(glue)
library(sf)

UASCs <- read_csv("UASCs.csv")

adm2 <- st_read("~/augmentedADM2s.geojson", stringsAsFactors = FALSE)

SNCs <- c("Unaccompanied Children" = "UC",
          "Separated Children" = "SC")

Genders <- c("Male", "Female")
Ages <- c(0, 18)
CoOs <- unique(UASCs$CoO)

ui <- fillPage(
  leafletOutput("map", height = "100%"),
  absolutePanel(
    top = 20,
    right = 20,
    height = "100%",
    wellPanel(
      radioButtons(
        "SNC",
        "Specific Needs Group",
        choices = SNCs,
        selected = SNCs[1],
        inline = TRUE
      ),
      hr(),
      sliderInput("Age", "Age", Ages[1], Ages[2], Ages, step = 1),
      hr(),
      checkboxGroupInput(
        "Gender",
        "Gender",
        choices = Genders,
        selected = Genders,
        inline = TRUE
      ),
      hr(),
      checkboxGroupInput(
        "CoO",
        "Country of Origin",
        choices = sort(CoOs),
        selected = CoOs
      )
    )
  )
)

server <- function(input, output) {
  mapData <- reactive({
    adm2 %>% left_join(
      UASCs %>% filter(
        SNC == input$SNC,
        Age %>% between(input$Age[1], input$Age[2]),
        Gender %in% input$Gender,
        CoO %in% input$CoO
      ) %>%
        count(Sec_Code)
    ) %>% replace_na(list(n = 0)) %>% filter(n > 0)
  })
  
  colorPalette <- reactive({
    colorNumeric("Blues", NULL)
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "https://{s}.tile.thunderforest.com/{variant}/{z}/{x}/{y}.png?apikey={apikey}",
        attribution = "&copy; <a href='http://www.thunderforest.com/'>Thunderforest</a>,  &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a>",
        options = tileOptions(variant = "transport", apikey = Sys.getenv("THUNDERFOREST_API_KEY"))
      ) %>%
      fitBounds(24.7, 22.0, 36.9, 31.7)
  })
  
  observe({
    pal <- colorPalette()
    leafletProxy("map", data = mapData()) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        color =  ~ pal(log10(n)),
        label =  ~ glue("{n} in {Sec_Name_En}, {Gov_Name_En}."),
        fillOpacity = .5
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values =  ~ n,
        title = glue("# of {input$SNC}")
      )
  })
}

shinyApp(ui = ui, server = server)
