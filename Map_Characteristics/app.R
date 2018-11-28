#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)

# Reads rds from the process_data file 
leafmap <- read_rds("./leafmap_rich.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Neighborhood Characteristics"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             sliderInput("rank", "Mean Income Rank of Residents Born into Household at 25 Percentile:", 0, 70, c(25, 50))
           )
    ),
    column(9,
           leafletOutput("mymap")
    )
  )
)

server <- function(input, output) {
  
  # Creates color palette that will stay the same regardless of subsetted data
  pal <- colorNumeric("RdYlGn", leafmap_rich$all)
  
  # Outputs basic parts of map that will not change with user input
  output$mymap <- renderLeaflet({
    
    leaflet(data = leafmap_rich) %>% 
      addTiles() %>%
      setView(lng = -96.35, lat = 37.45, zoom = 4)  %>%
      addLegend("bottomright", pal = pal, values = ~all,
                title = "Mean Income Rank (%)",
                opacity = 1)
    
  })
  
  # Creates observe function that will filter the data and display the counties selected by user input
  observe({
    min_rank <- input$rank[1]
    max_rank <- input$rank[2]
    
    filtered <- leafmap_rich[min_rank < leafmap_rich@data$all & leafmap_rich@data$all < max_rank, ]
    
    popup_dat <- paste0("<strong>County: </strong>",
                        leafmap$id,
                        "<br><strong>Mean Income Rank for Children Whose Parents Were at 25th Percentile: </strong>",
                        filtered$all, "%")
    
    leafletProxy("mymap", data = filtered) %>%
      clearShapes() %>%
      addPolygons(fillColor = ~pal(all),
                  fillOpacity = 0.8,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = popup_dat)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

