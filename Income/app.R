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
                                selected = c("male", "female")),
             sliderInput("alpha", "Opacity of points:",
                         0, 1, 0.5, step = .05)
           )
    ),
    column(9,
           h4(
             span(textOutput("title"), 
                  "vs. Mean Income Rank of Children Whose Parents Were at the 25th Percentile")
           ),
           plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    results <- tidy_all %>%
      filter(metric_type == "income") %>%
      filter(race %in% input$race) %>%
      filter(gender %in% input$gender) %>%
      mutate(value = value * 100)
    
    ggplot(results, aes_string(x = input$xvar, y = "value", color = "race", shape = "gender")) +
    geom_point(alpha = input$alpha) +
    labs(x = names(axis_vars)[axis_vars == input$xvar], y = "Mean Income Rank for Children Whose Parents Were at 25th Percentile")
  })
  
  output$title <- renderText({ names(axis_vars)[axis_vars == input$xvar] })
}

# Run the application 
shinyApp(ui = ui, server = server)

