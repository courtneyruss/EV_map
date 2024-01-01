# Install necessary packages if not installed
# install.packages(c("shiny", "leaflet", "dplyr", "DT"))

# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(stringr)
library(tools)

# Load data
data <- read.csv("EV_Roam_charging_stations.csv") %>%
  filter(latitude >= -47.0 & latitude <= -34.0 & longitude >= 166.0 & longitude <= 178.0) %>%
  select(name, latitude, longitude, maxTimeLimit, hasChargingCost, numberOfConnectors, currentType, address, operator) %>%
  mutate(
    maxTimeLimit = ifelse(maxTimeLimit %in% c('No Limit', 'None'), 'Unlimited', maxTimeLimit),
    Free = ifelse(hasChargingCost, 'No', 'Yes'),
    operator = str_replace_all(operator, "\\bNZ\\b", "NZ") %>%
      str_replace_all("\\b(?!NZ)\\w+\\b", ~str_to_title(.)),
    name = str_to_title(name),
    # Additional step to convert specific words to all caps
    across(c(name, operator), ~str_replace_all(., "\\b(ev|Ev|Bp|bp|Yha|Tlc|Hpc|Nz|nz)\\b", str_to_upper))
  ) %>%
  select(-hasChargingCost)

# Define UI
ui <- fluidPage(
  # Add a custom style for the title
  tags$head(
    tags$style(HTML("
      .title {
        font-size: 32px;
        font-weight: bold;
        color: #3498db;
        text-align: center;
        margin: 20px 0;
      }
      .sidebar {
        background-color: #ecf0f1;
        padding: 15px;
        border-radius: 10px;
        margin: 20px;
      }
      .filterSidebar {
        width: 100%;
        background-color: #ffffff; /* Same as infoBox */
        border: 2px solid #ccc;
        border-radius: 10px;
        padding: 15px;
        margin-bottom: 20px; /* Add margin between filter and info sections */
      }
      .infoBox {
        padding: 20px;
        background-color: #ffffff;
        border: 2px solid #ccc;
        border-radius: 10px;
        margin-bottom: 20px; /* Add margin between info and map sections */
      }
    "))
  ),
  # Title panel with custom styling
  titlePanel(
    div(
      class = "title",
      "New Zealand EV Charging Stations Map"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      div(
        class = "filterSidebar",
        checkboxInput("free_filter", "Show only Free Chargers", value = FALSE),
        checkboxInput("unlimited_filter", "Show only Unlimited Time Chargers", value = FALSE)
      ),
      div(
        class = "infoBox",
        div(
          id = "infoBox",
          HTML(paste0(
            "Data Source: ", tags$a(href = "https://catalogue.data.govt.nz/dataset/ev-roam-charging-stations3", "Waka Kotahi NZTA via data.govt.nz"), "<br> <br>",
            "<div style='font-size: 14px;'>",
            "App Developer: Courtney Russ", "<br>",
            tags$a(href = "https://github.com/courtneyruss/EV_map", "GitHub"), " | ",
            tags$a(href = "https://www.termsfeed.com/live/3dab9897-d45d-43d7-9775-2fd7ee340596", "Disclaimer"), " | ",
            tags$a(href = "https://www.datascienceportfol.io/CourtneyRuss", "Contact"),
            "</div><br>",
            "<p xmlns:cc='http://creativecommons.org/ns#' xmlns:dct='http://purl.org/dc/terms/'>",
            "<span property='dct:title'>NZ EV Charging Stations Map</span> by ",
            "<a rel='cc:attributionURL dct:creator' property='cc:attributionName' href='https://github.com/courtneyruss'>Courtney Russ</a> is marked with ",
            "<a href='http://creativecommons.org/publicdomain/zero/1.0?ref=chooser-v1' target='_blank' rel='license noopener noreferrer' style='display:inline-block;'>",
            "CC0 1.0<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/cc.svg?ref=chooser-v1'>",
            "<img style='height:22px!important;margin-left:3px;vertical-align:text-bottom;' src='https://mirrors.creativecommons.org/presskit/icons/zero.svg?ref=chooser-v1'></a></p>"
          ))
        )
      )
    ),
    
    mainPanel(
      leafletOutput("map", width = "100%", height = "600px")
    )
  ),
  
  # Add styling to the entire page
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        margin: 20px;
      }
      .container-fluid {
        border: 2px solid #ddd;
        border-radius: 10px;
        padding: 20px;
      }
    "))
  )
)

# Define server
server <- function(input, output, session) {
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    filtered_data <- data
    if (input$free_filter) {
      filtered_data <- filtered_data[filtered_data$Free == "Yes", ]
    }
    if (input$unlimited_filter) {
      filtered_data <- filtered_data[filtered_data$maxTimeLimit == "Unlimited", ]
    }
    
    leaflet(filtered_data) %>% 
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions(maxClusterRadius = 45),
                 popup = ~paste("Name: ", name, "<br>Address: ", address, "<br>Time Limit: ", maxTimeLimit, 
                                "<br>Free: ", Free, "<br>Operator: ", operator),
                 label = ~name, group = "stations") %>%
      setView(lng = 172.0, lat = -41.0, zoom = 5) 
  })
}

# Run the app
shinyApp(ui, server)
