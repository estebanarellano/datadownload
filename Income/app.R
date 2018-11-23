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
library(ggplot2)

tidy_all <- read_rds("tidy_all.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  titlePanel("Movie explorer"),
  fluidRow(
    column(3,
           wellPanel(
             h4("Filter"),
             selectInput("xvar", "X-axis:", axis_vars),
             checkboxGroupInput("race", "Race:", c("Black" = "black", 
                                                   "White" = "white", 
                                                   "Hispanic" = "hispanic"), 
                                selected = c("black", "white", "hisp")),
             checkboxGroupInput("gender", "Gender:", c("Male" = "male", 
                                                       "Female" = "female"),
                                selected = c("male", "female"))
           )
    ),
    column(9,
           plotOutput("distPlot"),
           wellPanel(
             span("Number of movies selected:",
                  textOutput("n_movies")
             )
           )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    raceplot <- tidy_all %>%
      filter(metric_type == "income") %>%
      filter(race %in% input$race) %>%
      filter(gender %in% input$gender) %>%
      ggplot(aes_string(x = input$xvar, y = "value", color = "race")) +
      geom_point()
    print(raceplot)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

