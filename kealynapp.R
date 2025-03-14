library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)

# Define UI for the dashboard
ui <- dashboardPage(
  dashboardHeader(title = "Soybeans In Virginia"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      # Leaflet Map Output
      box(width = 12, leafletOutput("soybeanMap", height = 500))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Data preparation: ensure count is an sf object with correct CRS
  count <- st_as_sf(count, wkt = "geometry", crs = 4326)  # Assuming 'geometry' column is in WKT format
  
  # Convert 'locations' to spatial data with proper CRS
  locations <- st_as_sf(locations, coords = c('lng', 'lat'), crs = 4326)
  
  # Define the color palette for Soybeans acres harvested
  pal <- colorNumeric(palette = c('white', 'orange', 'red4'), domain = count$Soybeans)
  
  # Title for the map
  tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title { 
      transform: translate(-50%, 20%);
      position: fixed !important;
      left: 50%;
      text-align: center;
      padding-left: 10px; 
      padding-right: 10px; 
      background: rgba(255,255,255,0.75);
      font-weight: bold;
      font-size: 28px;
    }
  "))
  
  title <- tags$div(tag.map.title, HTML('Soybeans Harvested'))
  
  # Render the leaflet map in the Shiny app
  output$soybeanMap <- renderLeaflet({
    leaflet(count) %>% 
      addTiles() %>%  # Base map tiles
      
      # Add county polygons with soybeans data
      addPolygons(weight = 1.0, smoothFactor = 1.0, opacity = 1.0, fillOpacity = 1.0, 
                  color = 'black', 
                  fillColor = ~pal(Soybeans), 
                  label = ~paste(NAME, Soybeans, 'Acres Harvested')) %>%
      
      # Add circle markers for crush plants
      addCircleMarkers(data = locations[locations$Type == 'Crush Plant',], 
                       group = 'Crush Plant', 
                       color = "darkgreen", 
                       fillColor = "darkgreen", 
                       fillOpacity = 1, 
                       radius = 4, 
                       label = ~paste(Place, '<br>', Capacity, 'Million bu per Year')) %>%
      
      # Add circle markers for grain elevators
      addCircleMarkers(data = locations[locations$Type == 'Grain Elevator',], 
                       group = 'Grain Elevator', 
                       color = "darkblue", 
                       fillColor = "darkblue", 
                       fillOpacity = 1, 
                       radius = 4, 
                       label = ~paste(Place)) %>%
      
      # Add circle markers for additional facilities
      addCircleMarkers(data = locations[locations$Type == 'Additional Facility',], 
                       group = 'Additional Facility', 
                       color = "yellow", 
                       fillColor = "yellow", 
                       fillOpacity = 1, 
                       radius = 4, 
                       label = ~paste(Place)) %>%
      
      # Add background map layer
      addProviderTiles('Esri.WorldGrayCanvas') %>%
      
      # Add legend for soybeans harvested (acres)
      addLegend(position = 'topright', 
                pal = pal, 
                values = count$Soybeans, 
                title = 'Soybeans Harvested <br> acres', 
                opacity = 1) %>%
      
      # Add legend for circle markers (Processing Facilities)
      addLegend(position = 'bottomright', 
                colors = c("darkgreen", "darkblue", "yellow"), 
                labels = c("Crush Plant", "Grain Elevator", "Additional Facilities"), 
                title = "Processing Facilities",
                opacity = 1) %>%
      
      # Add layers control for toggling facility types
      addLayersControl(overlayGroups = c('Crush Plant', 'Grain Elevator', 'Additional Facility'), 
                       options = layersControlOptions(collapsed = FALSE)) %>%
      
      # Add fixed title to the top-left of the map
      addControl(title, position = 'topleft', className = 'map-title')
  })
}

# Run the application
shinyApp(ui = ui, server = server)


