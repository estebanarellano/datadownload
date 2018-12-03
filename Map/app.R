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
leafmap <- read_rds("./leafmap.rds")

# Define UI for application that displays a map
ui <- fluidPage(
   
  titlePanel("Geographic Income Explorer by US County"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             
             # Input can include the race and gender of citizens included in the income analysis.
             
             checkboxGroupInput("race", "Race:", c("Black", "White", "Hispanic"), 
                                selected = c("Black", "White", "Hispanic")),
             checkboxGroupInput("gender", "Gender:", c("Male", "Female"),
                                selected = c("Male", "Female"))
           )
    ),
    column(9,
           h4(div("Mean Income Rank of Children Whose Parents Were at the 25th Percentile"
           )),
           leafletOutput("mymap")
    )
  )
)

# Define server logic required to create a Leaflet map
server <- function(input, output) {
  
  # Creates basic part of map that will not change with user input. For the sake of this particular Shiny, the map boundaries will not change nor will the tiles of the map.
  output$mymap <- renderLeaflet({
   
   leaflet() %>% 
     addTiles() %>%
     setView(lng = -96.35, lat = 37.45, zoom = 4)
   
 })
 
  # Creates observe function that will select the appropriate data to display based on user input. Because the input can be repeatedly change, an observe function is necessary to wait for these changes from the user.
  observe({
  
   # if/else if sequence that determines column to display, associate color palette, and text
    if (is.null(input$race) | is.null(input$gender)) {
      data <- leafmap$all
      pal <- colorNumeric("RdYlGn", data)
    }
    else if (input$race == c("Black", "White", "Hispanic") & input$gender == c("Male", "Female")) {
     data <- leafmap$all
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black, White, and Hispanic"
     gender_text <- "Men and Women"
   } else if (input$race == c("Black", "White") & input$gender == c("Male", "Female")) {
     data <- leafmap$all_black_white
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and White"
     gender_text <- "Men and Women"
   } else if (input$race == c("Black", "Hispanic") & input$gender == c("Male", "Female")) {
     data <- leafmap$all_black_hispanic
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and Hispanic"
     gender_text <- "Men and Women"
   } else if (input$race == "Black" & input$gender == c("Male", "Female")) {
     data <- leafmap$all_black
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black"
     gender_text <- "Men and Women"
   } else if (input$race == c("Black", "White", "Hispanic") & input$gender == "Male") {
     data <- leafmap$all_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black, White, and Hispanic"
     gender_text <- "Men"
   } else if (input$race == c("Black", "White") & input$gender == "Male") {
     data <- leafmap$black_white_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and White"
     gender_text <- "Men"
   } else if (input$race == c("Black", "Hispanic") & input$gender == "Male") {
     data <- leafmap$black_hispanic_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and Hispanic"
     gender_text <- "Men"
   } else if (input$race == "Black" & input$gender == "Male") {
     data <- leafmap$black_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black"
     gender_text <- "Men"
   } else if (input$race == c("Black", "White", "Hispanic") & input$gender == "Female") {
     data <- leafmap$all_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black, White, and Hispanic"
     gender_text <- "Women"
   } else if (input$race == c("Black", "White") & input$gender == "Female") {
     data <- leafmap$black_white_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and White"
     gender_text <- "Women"
   } else if (input$race == c("Black", "Hispanic") & input$gender == "Female") {
     data <- leafmap$black_hispanic_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black and Hispanic"
     gender_text <- "Women"
   } else if (input$race == "Black" & input$gender == "Female") {
     data <- leafmap$black_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Black"
     gender_text <- "Women"
   } else if (input$race == c("Hispanic", "White") & input$gender == c("Male", "Female")) {
     data <- leafmap$all_hispanic_white
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic and White"
     gender_text <- "Men and Women"
   } else if (input$race == c("Hispanic", "White") & input$gender == "Male") {
     data <- leafmap$hispanic_white_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic and White"
     gender_text <- "Men"
   } else if (input$race == c("Hispanic", "White") & input$gender == "Female") {
     data <- leafmap$hispanic_white_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic and White"
     gender_text <- "Women"
   } else if (input$race == "White" & input$gender == c("Male", "Female")) {
     data <- leafmap$all_white
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "White"
     gender_text <- "Men and Women"
   } else if (input$race == "White" & input$gender == "Male") {
     data <- leafmap$white_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "White"
     gender_text <- "Men"
   } else if (input$race == "White" & input$gender == "Female") {
     data <- leafmap$white_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "White"
     gender_text <- "Women"
   } else if (input$race == "Hispanic" & input$gender == c("Male", "Female")) {
     data <- leafmap$all_hispanic
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic"
     gender_text <- "Men and Women"
   } else if (input$race == "Hispanic" & input$gender == "Male") {
     data <- leafmap$hispanic_male
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic"
     gender_text <- "Men"
   } else if (input$race == "Hispanic" & input$gender == "Female") {
     data <- leafmap$hispanic_female
     pal <- colorNumeric("RdYlGn", data)
     race_text <- "Hispanic"
     gender_text <- "Women"
   }
   
   # Creates function to display data when a county is clicked
   popup_dat <- paste0("<strong>County: </strong>",
                       leafmap$id,
                       "<br><strong>Mean Income Rank: </strong>",
                       data, "%")
   
   # Leaflet proxy that only changes the appropriate parts of the map based on user input
   leafletProxy("mymap", data = leafmap) %>%
     clearShapes() %>%
     clearControls() %>%
     addPolygons(fillColor = pal(data),
                 fillOpacity = 0.8,
                 color = "#BDBDC3",
                 weight = 1,
                 popup = popup_dat) %>%
     addLegend("bottomright", pal = pal, values = data,
               title = "Mean Income Rank (%)",
               opacity = 1)
   
 })

}

# Run the application 
shinyApp(ui = ui, server = server)

