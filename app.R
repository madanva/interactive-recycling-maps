#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(leaflet)
library(rnaturalearth)
library(shiny)
library(rsconnect)
library(rnaturalearthhires)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$style(HTML("body { background-color: #f7eab7; }")),
  
  # Center-aligned title panel
  tags$div(
    style = "text-align: center;",
    titlePanel("Recycling Centers Interactive Map")
  ),
  
  # Leaflet map output
  div(
    style = "margin: 0 auto; width: 80%;",  # Center-align the map and adjust width
    leafletOutput("map")
  ),
  
  # Footer
  tags$footer(
    style = "text-align: center; padding: 10px;",
    HTML("Developed by Varun Madan<br><small>Data from ENF recycling, blank map interface from Leaflet, and hosting from shinyapps.io.</small>")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  recycling_centers <- read.csv("recycling_centers.csv")
  
    output$map <- renderLeaflet({
      
      
      # Calculate the center coordinates for the specified countries
      center_lng <- mean(c(-0.1278, 10.4515, 10.7485))  # Mean of longitudes 
      center_lat <- mean(c(51.5074, 51.1657, 50.1657))  # Mean of latitudes
      
      # Create leaflet map with OSM tiles
      map <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        setView(lng = center_lng, lat = center_lat, zoom = 4.3)  # Adjust the zoom level
      
      # Add marker points for recycling centers with dynamic popups
      for (i in 1:nrow(recycling_centers)) {
        popup <- paste(
          "<strong>Name:</strong>", recycling_centers$name[i],
          "<br><strong>Accepted Materials:</strong>", recycling_centers$accepted_materials[i]
        )
        
        if (!is.na(recycling_centers$recycled_products[i]) && nchar(trimws(recycling_centers$recycled_products[i])) > 0) {
          popup <- paste(popup, "<br><strong>Recycled Products:</strong>", recycling_centers$recycled_products[i])
        }
        
        if (!is.na(recycling_centers$materials_processed[i]) && nchar(trimws(recycling_centers$materials_processed[i])) > 0) {
          popup <- paste(popup, "<br><strong>Materials Processed (tons/year):</strong>", recycling_centers$materials_processed[i])
        }
        
        if (!is.na(recycling_centers$capacity[i]) && nchar(trimws(recycling_centers$capacity[i])) > 0) {
          popup <- paste(popup, "<br><strong>Capacity (tons/year):</strong>", recycling_centers$capacity[i])
        }
        
        map <- map %>%
          addMarkers(
            lng = recycling_centers$longitude[i],
            lat = recycling_centers$latitude[i],
            popup = popup
          )
      }
      
      france_border <- ne_countries(country = "France", scale = "large", returnclass = "sf")
      germany_border <- ne_countries(country = "Germany", scale = "large", returnclass = "sf")
      gb_border <- ne_countries(country = "United Kingdom", scale = "large", returnclass = "sf")
      canada_border <- ne_countries(country = "Canada", scale = "large", returnclass = "sf")
      
      map <- map %>%
        addPolygons(data = france_border, color = "#3037f2", weight = 2) %>%
        addPolygons(data = germany_border, color = "#deb710", weight = 2) %>%
        addPolygons(data = gb_border, color = "#de10c9", weight = 2) %>%
        addPolygons(data = canada_border, color = "#bd1321", weight = 2)
      
      
      # Display the map
      return(map)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
